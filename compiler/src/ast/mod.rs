use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::PrattParser,
    Span,
};

use crate::parser::Rule;

pub trait Node<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self;
}

#[derive(Debug)]
pub struct SourceFile<'i> {
    pub top_level: Vec<TopLevelDecl<'i>>,
}

#[derive(Debug)]
pub enum TopLevelDecl<'i> {
    FnDecl(FnDecl<'i>),
}

#[derive(Debug)]
pub struct FnDecl<'i> {
    pub name: Ident<'i>,
    pub params: Vec<FnParameterDecl<'i>>,
    pub result_kind: Option<Kind<'i>>,
    pub body: Vec<Statement<'i>>,
    pub span: Span<'i>,
}

impl<'i> Node<'i> for FnDecl<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        if pair.as_rule() != Rule::FunctionDecl {
            unreachable!("FnDecl has been checked");
        }

        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let name = Ident::parse(pairs.next().unwrap());
        let signature = pairs.next().unwrap();
        let body_pair = pairs.next().unwrap();

        let mut sig_children = signature.into_inner();
        let param_pair = sig_children.next().unwrap();
        let result_kind = sig_children.next().map(Kind::parse);
        let mut params = vec![];
        if let Some(param_list_pair) = param_pair.into_inner().next() {
            let param_list = ParameterList::parse(param_list_pair);
            let param_decls = param_list.0;
            for param_decl in &param_decls {
                if param_decl.name.is_none() {
                    panic!("cant have only type in fn decl params")
                }
            }
            params = param_decls.into_iter().map(|p| p.into()).collect();
        }

        let body = body_pair.into_inner().map(Statement::parse).collect();

        Self {
            name,
            params,
            result_kind,
            body,
            span,
        }
    }
}

#[derive(Debug)]
pub struct ParameterList<'i>(pub Vec<ParameterDecl<'i>>);

impl<'i> Node<'i> for ParameterList<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        if pair.as_rule() != Rule::ParameterList {
            unreachable!("FnDecl has been checked");
        }

        let mut params = vec![];
        for param_decl in pair.into_inner() {
            let mut pairs = param_decl.into_inner();
            let first = pairs.next().unwrap();
            match first.as_rule() {
                // idents with types
                Rule::IdentifierList => {
                    let second = pairs.next().unwrap();

                    for ident in first.into_inner() {
                        let span = ident.as_span();
                        params.push(ParameterDecl {
                            name: Some(Ident::parse(ident)),
                            kind: Kind::parse(second.clone()),
                            span,
                        });
                    }
                }
                // only types
                Rule::Type => {
                    let span = first.as_span();
                    params.push(ParameterDecl {
                        name: None,
                        kind: Kind::parse(first),
                        span,
                    });
                    for kind in pairs {
                        let span = kind.as_span();
                        params.push(ParameterDecl {
                            name: None,
                            kind: Kind::parse(kind),
                            span,
                        });
                    }
                }
                _ => unreachable!("parameter list should not have anything else"),
            }
        }
        Self(params)
    }
}

#[derive(Debug)]
pub struct Ident<'i> {
    pub value: String,
    pub span: Span<'i>,
}

impl<'i> Node<'i> for Ident<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        if pair.as_rule() != Rule::Ident {
            unreachable!("Ident has been checked");
        }

        Self {
            value: pair.as_str().to_owned(),
            span: pair.as_span(),
        }
    }
}

#[derive(Debug)]
pub struct ParameterDecl<'i> {
    pub name: Option<Ident<'i>>,
    pub kind: Kind<'i>,
    pub span: Span<'i>,
}

#[derive(Debug)]
pub struct FnParameterDecl<'i> {
    pub name: Ident<'i>,
    pub kind: Kind<'i>,
    pub span: Span<'i>,
}

impl<'i> From<ParameterDecl<'i>> for FnParameterDecl<'i> {
    fn from(value: ParameterDecl<'i>) -> Self {
        Self {
            name: value.name.unwrap(),
            kind: value.kind,
            span: value.span,
        }
    }
}

#[derive(Debug)]
pub enum Kind<'i> {
    Simple {
        name: String,
        span: Span<'i>,
    },
    Tuple {
        kinds: Vec<Kind<'i>>,
        span: Span<'i>,
    },
}

impl<'i> Node<'i> for Kind<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        if pair.as_rule() != Rule::Type {
            unreachable!("Type is already checked");
        }

        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::Ident => Self::Simple {
                name: inner.as_str().to_owned(),
                span: inner.as_span(),
            },
            Rule::Tuple => {
                let inner = inner.into_inner().next().unwrap();
                let span = inner.as_span();
                let mut kinds = Vec::new();
                match inner.as_rule() {
                    Rule::Type => {
                        kinds.push(Kind::parse(inner));
                    }
                    Rule::TypeList => {
                        for inner in inner.into_inner() {
                            kinds.push(Kind::parse(inner));
                        }
                    }
                    _ => unreachable!(""),
                }
                Self::Tuple { kinds, span }
            }
            _ => unreachable!(""),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    Star,
    And,
}

#[derive(Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    Pipe,
    Percent,
    And,
}

#[derive(Debug)]
pub enum Expr<'i> {
    Identifier {
        value: &'i str,
        span: Span<'i>,
    },
    Integer {
        value: i32,
        span: Span<'i>,
    },
    FunctionCall {
        function: Box<Expr<'i>>,
        args: Vec<Expr<'i>>,
    },
    UnaryExpr {
        op: UnaryOp,
        operand: Box<Expr<'i>>,
    },
    BinaryExpr {
        left: Box<Expr<'i>>,
        op: BinaryOp,
        right: Box<Expr<'i>>,
    },
}

impl<'i> Node<'i> for Expr<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        match pair.as_rule() {
            Rule::Ident => Expr::Identifier {
                value: pair.as_str(),
                span: pair.as_span(),
            },
            Rule::IntLit => Expr::Integer {
                value: pair.as_str().parse().unwrap(),
                span: pair.as_span(),
            },
            Rule::FunctionCall => Self::parse_expr(pair.into_inner()),
            _ => unreachable!(),
        }
    }
}

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(
                Op::infix(Rule::Eq, Left) | Op::infix(NotEq, Left) | Op::infix(Le, Left) |
                Op::infix(Lt, Left) | Op::infix(Ge, Left) | Op::infix(Gt, Left)
            )
            .op(Op::infix(Plus, Left) | Op::infix(Minus, Left) | Op::infix(Pipe, Left))
            .op(
                Op::infix(Star, Left) | Op::infix(Slash, Left) | Op::infix(Percent, Left) |
                Op::infix(And, Left)
            )
            .op(
                Op::prefix(UnaryPlus) | Op::prefix(UnaryMinus) | Op::prefix(UnaryNot) |
                Op::prefix(UnaryStar) | Op::prefix(UnaryAnd)
            )
            .op(Op::postfix(Arguments))
    };
}

impl<'i> Expr<'i> {
    fn parse_expr<I: Iterator<Item = Pair<'i, Rule>>>(pairs: I) -> Expr<'i> {
        PRATT_PARSER
            .map_primary(|primary| match primary.as_rule() {
                Rule::IntLit | Rule::Ident | Rule::FunctionCall => Expr::parse(primary),
                _ => unreachable!(),
            })
            .map_infix(|left, op, right| {
                let op = match op.as_rule() {
                    Rule::Pipe => BinaryOp::Pipe,
                    Rule::Plus => BinaryOp::Plus,
                    Rule::Minus => BinaryOp::Minus,
                    Rule::Star => BinaryOp::Star,
                    Rule::Percent => BinaryOp::Percent,
                    Rule::And => BinaryOp::And,
                    Rule::Slash => BinaryOp::Slash,
                    _ => unreachable!(""),
                };
                Expr::BinaryExpr {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                }
            })
            .map_prefix(|op, right| {
                let op = match op.as_rule() {
                    Rule::UnaryMinus => UnaryOp::Minus,
                    Rule::UnaryStar => UnaryOp::Star,
                    Rule::UnaryPlus => UnaryOp::Plus,
                    Rule::UnaryAnd => UnaryOp::And,
                    Rule::UnaryNot => UnaryOp::Not,
                    _ => unreachable!(""),
                };
                Expr::UnaryExpr {
                    op,
                    operand: Box::new(right),
                }
            })
            .map_postfix(|left, op| match op.as_rule() {
                Rule::Arguments => {
                    // TODO: handle when first arg is 'Kind'
                    Expr::FunctionCall {
                        function: Box::new(left),
                        args: op
                            .into_inner()
                            .next()
                            .map(Self::parse_expr_list)
                            .unwrap_or_else(|| vec![]),
                    }
                }
                _ => unreachable!(""),
            })
            .parse(pairs)
    }

    fn parse_expr_list(pairs: Pair<'i, Rule>) -> Vec<Expr<'i>> {
        // having this inside the 'map_postfix()' of parse_expr() leads to a compile
        // time recursion limit error.
        let mut inner = pairs.into_inner();
        let mut args = vec![];
        while inner.peek().is_some() {
            let inner_expr = inner
                .by_ref()
                .take_while(|pair| pair.as_rule() != Rule::Comma);
            args.push(Expr::parse_expr(inner_expr));
        }
        args
    }
}

#[derive(Debug)]
pub enum Statement<'i> {
    // TODO: add expression list
    Return,
    Expression(Expr<'i>),
}

impl<'i> Node<'i> for Statement<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Self {
        match pair.as_rule() {
            Rule::ExpressionStmt => {
                let expr = Expr::parse_expr(pair.into_inner());
                Statement::Expression(expr)
            }
            Rule::ReturnStmt => Statement::Return,
            _ => unreachable!(),
        }
    }
}
