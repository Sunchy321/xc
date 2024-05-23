use std::cmp::Ordering;
use std::collections::HashMap;

use xc_span::symbol::{kw, op};
use xc_span::Symbol;

#[derive(Clone, Debug)]
pub struct OpContext {
    infix_map: HashMap<Symbol, OpInfo>,
    prefixes: Vec<Symbol>,
    suffixes: Vec<Symbol>,
}

impl OpContext {
    /// create operator context with builtin operators
    pub fn new() -> OpContext {
        let prefixes = vec![op::Plus, op::Minus, op::BitNegate, kw::Some];
        let suffixes = vec![op::Question, op::Exclamation];

        let mut map = HashMap::new();

        let mut iter = 0u32..;

        let multiplicative = OpInfo::new(iter.next().unwrap(), Associativity::Left);

        map.insert(op::Multiply, multiplicative);
        map.insert(op::Divide, multiplicative);
        map.insert(op::Modulo, multiplicative);

        let additive = OpInfo::new(iter.next().unwrap(), Associativity::Left);

        map.insert(op::Plus, additive);
        map.insert(op::Minus, additive);

        let shift = OpInfo::new(iter.next().unwrap(), Associativity::None);

        map.insert(kw::Shl, shift);
        map.insert(kw::Shr, shift);

        let bitwise = OpInfo::new(iter.next().unwrap(), Associativity::Isolate);

        map.insert(op::BitAnd, bitwise);
        map.insert(op::BitXor, bitwise);
        map.insert(op::BitOr, bitwise);

        let range = OpInfo::new(iter.next().unwrap(), Associativity::None);

        map.insert(op::Range, range);
        map.insert(op::ClosedRange, range);

        let concat = OpInfo::new(iter.next().unwrap(), Associativity::Isolate);

        map.insert(op::Concat, concat);

        let null_coalescing = OpInfo::new(iter.next().unwrap(), Associativity::Isolate);

        map.insert(op::NullCoalescing, null_coalescing);

        let comparison = OpInfo::new(iter.next().unwrap(), Associativity::Comparison);

        map.insert(kw::Cmp, comparison);
        map.insert(op::Equal, comparison);
        map.insert(op::NotEqual, comparison);
        map.insert(op::Less, comparison);
        map.insert(op::NotLess, comparison);
        map.insert(op::Greater, comparison);
        map.insert(op::NotGreater, comparison);
        map.insert(op::LessEqual, comparison);
        map.insert(op::GreaterEqual, comparison);
        map.insert(op::LessGreater, comparison);

        map.insert(kw::In, comparison);

        let logical = OpInfo::new(iter.next().unwrap(), Associativity::Isolate);

        map.insert(op::And, logical);
        map.insert(op::Or, logical);

        let assignment = OpInfo::new(iter.next().unwrap(), Associativity::None);

        map.insert(op::Assign, assignment);
        map.insert(op::PlusAssign, assignment);
        map.insert(op::SubtractAssign, assignment);
        map.insert(op::MultiplyAssign, assignment);
        map.insert(op::DivideAssign, assignment);
        map.insert(op::ModuloAssign, assignment);
        map.insert(kw::ShlEq, assignment);
        map.insert(kw::ShrEq, assignment);
        map.insert(op::BitAndAssign, assignment);
        map.insert(op::BitXorAssign, assignment);
        map.insert(op::BitOrAssign, assignment);
        map.insert(op::NullCoalescingAssign, assignment);
        map.insert(op::ConcatLeft, assignment);
        map.insert(op::ConcatRight, assignment);

        Self {
            infix_map: map,
            prefixes,
            suffixes,
        }
    }

    pub fn is_prefix_op(&self, sym: Symbol) -> bool {
        self.prefixes.contains(&sym)
    }

    pub fn is_suffix_op(&self, sym: Symbol) -> bool {
        self.suffixes.contains(&sym)
    }

    pub fn is_infix_op(&self, sym: Symbol) -> bool {
        self.infix_map.contains_key(&sym)
    }

    pub fn is_op(&self, sym: Symbol) -> bool {
        self.is_prefix_op(sym) || self.is_suffix_op(sym) || self.is_infix_op(sym)
    }

    pub fn get_info(&self, sym: Symbol) -> Option<OpInfo> {
        self.infix_map.get(&sym).copied()
    }

    pub fn precedence(&self, sym: Symbol) -> Option<Precedence> {
        self.infix_map.get(&sym).map(|info| info.precedence)
    }

    pub fn associativity(&self, sym: Symbol) -> Option<Associativity> {
        self.infix_map.get(&sym).map(|info| info.assoc)
    }

    pub fn equal_precedence(&self) -> u32 {
        if let Some(Precedence::Normal(value)) = self.precedence(op::Equal) {
            value
        } else {
            panic!("no precedence of `=` operator provided")
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct OpInfo {
    pub precedence: Precedence,
    pub assoc: Associativity,
}

impl OpInfo {
    pub fn new(precedence: u32, assoc: Associativity) -> OpInfo {
        OpInfo {
            precedence: Precedence::new(precedence),
            assoc,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Associativity {
    /// left associative, `a + b - c` is equivalent to `(a + b) - c`
    Left,
    /// right associative, `a + b - c` is equivalent to `a + (b - c)`
    Right,
    /// no mix, `a + b + c` is ok but `a + b - c` is an error
    Isolate,
    /// non associative, `a + b + c` is an error
    None,
    /// comparison operators has special associativity
    Comparison,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Precedence {
    Normal(u32),
    Individual,
}

impl Precedence {
    pub fn new(value: u32) -> Self {
        Self::Normal(value)
    }

    pub fn partial_cmp(&self, other: &Self, ctx: &OpContext) -> Option<Ordering> {
        use Precedence::*;

        match (self, other) {
            (Individual, Individual) => Some(Ordering::Equal),
            (Normal(left), Normal(right)) => left.partial_cmp(right),
            (Individual, Normal(right)) => {
                if *right >= ctx.equal_precedence() {
                    Some(Ordering::Less)
                } else {
                    None
                }
            }
            (Normal(left), Individual) => {
                if *left >= ctx.equal_precedence() {
                    Some(Ordering::Greater)
                } else {
                    None
                }
            }
        }
    }
}
