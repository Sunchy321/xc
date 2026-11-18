use xc_span::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedExpression {
    pub span: Span,
    pub type_name: String,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TypeInfo {
    expressions: Vec<TypedExpression>,
    unavailable_expressions: Vec<Span>,
}

impl TypeInfo {
    pub(crate) fn new(
        expressions: Vec<TypedExpression>,
        unavailable_expressions: Vec<Span>,
    ) -> Self {
        Self {
            expressions,
            unavailable_expressions,
        }
    }

    /// Return the narrowest inferred expression type covering a source byte offset.
    /// A narrower expression without a known type prevents a parent type from
    /// being reported for the child expression under the cursor.
    pub fn expression_at(&self, offset: u32) -> Option<&TypedExpression> {
        let expression = self
            .expressions
            .iter()
            .filter(|expression| {
                !expression.span.is_dummy()
                    && expression.span.lo.0 <= offset
                    && offset < expression.span.hi.0
            })
            .min_by_key(|expression| expression.span.hi.0 - expression.span.lo.0)?;
        let expression_width = expression.span.hi.0 - expression.span.lo.0;
        if self.unavailable_expressions.iter().any(|span| {
            !span.is_dummy()
                && span.lo.0 <= offset
                && offset < span.hi.0
                && span.hi.0 - span.lo.0 <= expression_width
        }) {
            return None;
        }
        Some(expression)
    }

    pub fn type_at(&self, offset: u32) -> Option<&str> {
        self.expression_at(offset)
            .map(|expression| expression.type_name.as_str())
    }
}
