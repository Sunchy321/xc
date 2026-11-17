#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Ty {
    Void,
    Never,
    Bool,
    Int,
    String,
    Array(Box<Ty>),
    Dictionary { key: Box<Ty>, value: Box<Ty> },
    Optional(Box<Ty>),
    Reference { target: Box<Ty>, mutable: bool },
    Function { params: Vec<Ty>, ret: Box<Ty> },
    Union(Vec<Ty>),
    Var(u32),
    Unknown,
}

pub(crate) fn from_ast(ty: &xc_ast::ty::Type) -> Ty {
    use xc_ast::ty::TypeKind;
    match &ty.kind {
        TypeKind::Void => Ty::Void,
        TypeKind::Never => Ty::Never,
        TypeKind::Bool => Ty::Bool,
        TypeKind::Int(None, _, _) => Ty::Int,
        TypeKind::String => Ty::String,
        TypeKind::Optional(inner) => Ty::Optional(Box::new(from_ast(inner))),
        TypeKind::Array(element) => match from_ast(element) {
            Ty::Unknown => Ty::Unknown,
            element => Ty::Array(Box::new(element)),
        },
        TypeKind::Dict(value, key) => Ty::Dictionary {
            key: Box::new(from_ast(key)),
            value: Box::new(from_ast(value)),
        },
        TypeKind::Reference(target, mutability) => Ty::Reference {
            target: Box::new(from_ast(target)),
            mutable: matches!(mutability, xc_ast::Mutability::Mut),
        },
        TypeKind::Paren(inner) => from_ast(inner),
        TypeKind::Fn(inputs, output) => match function_params(inputs) {
            Some(params) => Ty::Function {
                params,
                ret: Box::new(from_ast(output)),
            },
            None => Ty::Unknown,
        },
        _ => Ty::Unknown,
    }
}

fn function_params(ty: &xc_ast::ty::Type) -> Option<Vec<Ty>> {
    use xc_ast::ty::{Member, TypeKind};
    match &ty.kind {
        TypeKind::Void => Some(Vec::new()),
        TypeKind::Paren(inner) => function_params(inner),
        TypeKind::Struct(struct_ty) => struct_ty
            .members
            .iter()
            .map(|member| match member {
                Member::Ordinal(param, _) => Some(from_ast(param)),
                Member::Named(..) => None,
            })
            .collect(),
        _ => Some(vec![from_ast(ty)]),
    }
}

impl std::fmt::Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ty::Void => f.write_str("void"),
            Ty::Never => f.write_str("never"),
            Ty::Bool => f.write_str("bool"),
            Ty::Int => f.write_str("int"),
            Ty::String => f.write_str("string"),
            Ty::Array(element) => match element.as_ref() {
                Ty::Union(_) | Ty::Function { .. } => write!(f, "({element})[]"),
                _ => write!(f, "{element}[]"),
            },
            Ty::Dictionary { key, value } => match value.as_ref() {
                Ty::Union(_) | Ty::Function { .. } => write!(f, "({value})[{key}]"),
                _ => write!(f, "{value}[{key}]"),
            },
            Ty::Optional(inner) => match inner.as_ref() {
                Ty::Union(_) | Ty::Function { .. } => write!(f, "({inner})?"),
                _ => write!(f, "{inner}?"),
            },
            Ty::Reference { target, mutable } => {
                if *mutable {
                    write!(f, "{target} mut&")
                } else {
                    write!(f, "{target}&")
                }
            }
            Ty::Function { params, ret } => {
                f.write_str("(")?;
                for (index, param) in params.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{param}")?;
                }
                if matches!(ret.as_ref(), Ty::Union(_)) {
                    write!(f, ") -> ({ret})")
                } else {
                    write!(f, ") -> {ret}")
                }
            }
            Ty::Union(members) => {
                for (index, member) in members.iter().enumerate() {
                    if index != 0 {
                        f.write_str(" | ")?;
                    }
                    if matches!(member, Ty::Function { .. }) {
                        write!(f, "({member})")?;
                    } else {
                        write!(f, "{member}")?;
                    }
                }
                Ok(())
            }
            Ty::Var(id) => write!(f, "?{id}"),
            Ty::Unknown => f.write_str("unknown"),
        }
    }
}

pub(crate) fn is_subtype(a: &Ty, b: &Ty) -> bool {
    if a == b {
        return true;
    }
    match (a, b) {
        (Ty::Unknown, _) | (_, Ty::Unknown) => false,
        (Ty::Never, _) | (_, Ty::Void) => true,
        (Ty::Union(members), _) => members.iter().all(|member| is_subtype(member, b)),
        (_, Ty::Union(members)) => members.iter().any(|member| is_subtype(a, member)),
        (Ty::Array(left), Ty::Array(right)) => left == right,
        (
            Ty::Dictionary {
                key: left_key,
                value: left_value,
            },
            Ty::Dictionary {
                key: right_key,
                value: right_value,
            },
        ) => left_key == right_key && left_value == right_value,
        (Ty::Optional(left), Ty::Optional(right)) => is_subtype(left, right),
        (value, Ty::Optional(inner)) => is_subtype(value, inner),
        (
            Ty::Reference {
                target: left_target,
                mutable: left_mutable,
            },
            Ty::Reference {
                target: right_target,
                mutable: right_mutable,
            },
        ) => left_mutable == right_mutable && left_target == right_target,
        (
            Ty::Function {
                params: left_params,
                ret: left_ret,
            },
            Ty::Function {
                params: right_params,
                ret: right_ret,
            },
        ) => {
            left_params.len() == right_params.len()
                && left_params
                    .iter()
                    .zip(right_params)
                    .all(|(left, right)| is_subtype(right, left))
                && is_subtype(left_ret, right_ret)
        }
        _ => false,
    }
}

pub(crate) fn common_type(left: Ty, right: Ty) -> Ty {
    if matches!(&left, Ty::Unknown) || matches!(&right, Ty::Unknown) {
        return Ty::Unknown;
    }
    if is_subtype(&left, &right) {
        return right;
    }
    if is_subtype(&right, &left) {
        return left;
    }

    let mut members = Vec::new();
    flatten_union(left, &mut members);
    flatten_union(right, &mut members);
    members.sort();
    members.dedup();
    let candidates = members.clone();
    members.retain(|member| {
        !candidates
            .iter()
            .any(|other| member != other && is_subtype(member, other))
    });
    match members.len() {
        0 => Ty::Never,
        1 => members.pop().unwrap(),
        _ => Ty::Union(members),
    }
}

fn flatten_union(ty: Ty, output: &mut Vec<Ty>) {
    match ty {
        Ty::Union(members) => {
            for member in members {
                flatten_union(member, output);
            }
        }
        other => output.push(other),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn common_type_handles_never_void_and_unrelated_types() {
        assert_eq!(common_type(Ty::Never, Ty::Int), Ty::Int);
        assert_eq!(common_type(Ty::Int, Ty::Void), Ty::Void);
        assert_eq!(
            common_type(Ty::Int, Ty::String),
            Ty::Union(vec![Ty::Int, Ty::String])
        );
    }

    #[test]
    fn arrays_are_invariant_and_unknown_propagates() {
        assert_eq!(
            common_type(
                Ty::Array(Box::new(Ty::Int)),
                Ty::Array(Box::new(Ty::String))
            ),
            Ty::Union(vec![
                Ty::Array(Box::new(Ty::Int)),
                Ty::Array(Box::new(Ty::String)),
            ])
        );
        assert_eq!(common_type(Ty::Unknown, Ty::Int), Ty::Unknown);
        assert!(!is_subtype(&Ty::Unknown, &Ty::Int));
    }

    #[test]
    fn compound_type_display_preserves_precedence() {
        let element = Ty::Union(vec![Ty::Int, Ty::String]);
        assert_eq!(Ty::Array(Box::new(element)).to_string(), "(int | string)[]");
        assert_eq!(
            Ty::Array(Box::new(Ty::Function {
                params: vec![],
                ret: Box::new(Ty::Int),
            }))
            .to_string(),
            "(() -> int)[]"
        );
        assert_eq!(
            Ty::Function {
                params: vec![],
                ret: Box::new(Ty::Union(vec![Ty::Int, Ty::String])),
            }
            .to_string(),
            "() -> (int | string)"
        );
    }
}
