use pest::{iterators::Pair, pratt_parser::PrattParser, Span};

use crate::parser::Rule;

use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BakugoParsingErrorKind {
    #[error("Internal error")]
    InternalError,
    #[error("Syntax error")]
    SyntaxError,
}

#[derive(Error, Debug)]
#[error("{kind}: {msg}")]
pub struct BakugoParsingError<'i> {
    span: Span<'i>,
    msg: String,
    kind: BakugoParsingErrorKind,
}

impl<'i> BakugoParsingError<'i> {
    fn new(span: Span<'i>, msg: String, kind: BakugoParsingErrorKind) -> Self {
        Self { span, msg, kind }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("Parsing error")]
pub struct BakugoParsingErrorDisplay {
    #[source_code]
    src: NamedSource,

    #[label = "{kind}: {msg}"]
    span: SourceSpan,
    msg: String,

    kind: BakugoParsingErrorKind,
}

impl BakugoParsingErrorDisplay {
    pub fn from_error(err: BakugoParsingError, src: NamedSource) -> Self {
        Self {
            src,
            msg: err.msg,
            span: SourceSpan::new(
                err.span.start().into(),
                (err.span.end() - err.span.start()).into(),
            ),
            kind: err.kind,
        }
    }
}

pub trait Node<'i>
where
    Self: Sized,
{
    fn parse(pair: Pair<'i, Rule>) -> Result<Self, BakugoParsingError>;
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

fn check_rule<'i>(
    pair: &Pair<'i, Rule>,
    rule: Rule,
    rule_name: &str,
) -> Result<(), BakugoParsingError<'i>> {
    if pair.as_rule() != rule {
        Err(BakugoParsingError::new(
            pair.as_span(),
            format!("We ran into an error. Expected to see a {rule_name}."),
            BakugoParsingErrorKind::InternalError,
        ))
    } else {
        Ok(())
    }
}

impl<'i> Node<'i> for FnDecl<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Result<Self, BakugoParsingError> {
        check_rule(&pair, Rule::FunctionDecl, "functional declaration")?;

        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let name = Ident::parse(pairs.next().unwrap())?;
        let signature = pairs.next().unwrap();
        let body_pair = pairs.next().unwrap();

        let mut sig_children = signature.into_inner();
        let param_pair = sig_children.next().unwrap();
        let result_kind = sig_children.next().map(Kind::parse).transpose()?;
        let mut params = vec![];
        if let Some(param_list_pair) = param_pair.into_inner().next() {
            let param_list = ParameterList::parse(param_list_pair)?;
            let param_decls = param_list.0;
            for param_decl in &param_decls {
                if param_decl.name.is_none() {
                    return Err(BakugoParsingError::new(
                        param_decl.span,
                        "Function declaration parameters cannot only have a type".to_owned(),
                        BakugoParsingErrorKind::SyntaxError,
                    ));
                }
            }
            params = param_decls.into_iter().map(|p| p.into()).collect();
        }

        let body = body_pair
            .into_inner()
            .filter(|p| p.as_rule() != Rule::Semicolon)
            .map(Statement::parse)
            .collect::<Result<_, _>>()?;

        Ok(Self {
            name,
            params,
            result_kind,
            body,
            span,
        })
    }
}

#[derive(Debug)]
pub struct ParameterList<'i>(pub Vec<ParameterDecl<'i>>);

impl<'i> Node<'i> for ParameterList<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Result<Self, BakugoParsingError> {
        check_rule(&pair, Rule::ParameterList, "parameters")?;

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
                            name: Some(Ident::parse(ident)?),
                            kind: Kind::parse(second.clone())?,
                            span,
                        });
                    }
                }
                // only types
                Rule::Type => {
                    let span = first.as_span();
                    params.push(ParameterDecl {
                        name: None,
                        kind: Kind::parse(first)?,
                        span,
                    });
                    for kind in pairs {
                        let span = kind.as_span();
                        params.push(ParameterDecl {
                            name: None,
                            kind: Kind::parse(kind)?,
                            span,
                        });
                    }
                }
                _ => {
                    return Err(BakugoParsingError::new(
                        first.as_span(),
                        "Expected only types and identifiers in parameters".to_owned(),
                        BakugoParsingErrorKind::InternalError,
                    ))
                }
            }
        }
        Ok(Self(params))
    }
}

#[derive(Debug)]
pub struct Ident<'i> {
    pub value: String,
    pub span: Span<'i>,
}

impl<'i> Node<'i> for Ident<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Result<Self, BakugoParsingError> {
        check_rule(&pair, Rule::Ident, "identifiers")?;

        Ok(Self {
            value: pair.as_str().to_owned(),
            span: pair.as_span(),
        })
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
    fn parse(pair: Pair<'i, Rule>) -> Result<Self, BakugoParsingError> {
        check_rule(&pair, Rule::Type, "type")?;

        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::Ident => Ok(Self::Simple {
                name: inner.as_str().to_owned(),
                span: inner.as_span(),
            }),
            Rule::Tuple => {
                let inner = inner.into_inner().next().unwrap();
                let span = inner.as_span();
                let mut kinds = Vec::new();
                match inner.as_rule() {
                    Rule::Type => {
                        kinds.push(Kind::parse(inner)?);
                    }
                    Rule::TypeList => {
                        for inner in inner.into_inner() {
                            kinds.push(Kind::parse(inner)?);
                        }
                    }
                    _ => {
                        return Err(BakugoParsingError::new(
                            span,
                            "expected type but got something else".to_owned(),
                            BakugoParsingErrorKind::InternalError,
                        ))
                    }
                }
                Ok(Self::Tuple { kinds, span })
            }
            _ => {
                return Err(BakugoParsingError::new(
                    inner.as_span(),
                    "expected type but got something else".to_owned(),
                    BakugoParsingErrorKind::InternalError,
                ))
            }
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
        args: Args<'i>,
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
    fn parse(pair: Pair<'i, Rule>) -> Result<Self, BakugoParsingError> {
        match pair.as_rule() {
            Rule::Ident => Ok(Expr::Identifier {
                value: pair.as_str(),
                span: pair.as_span(),
            }),
            Rule::IntLit => Ok(Expr::Integer {
                value: pair.as_str().parse().unwrap(),
                span: pair.as_span(),
            }),
            Rule::FunctionCall => Ok(Self::parse_expr(pair.into_inner())?),
            _ => Err(BakugoParsingError::new(
                pair.as_span(),
                "got a non expression".to_owned(),
                BakugoParsingErrorKind::InternalError,
            )),
        }
    }
}

#[derive(Debug)]
pub struct Args<'i> {
    pub kind: Option<Kind<'i>>,
    pub args: Vec<Expr<'i>>,
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
    fn parse_expr<I: Iterator<Item = Pair<'i, Rule>>>(
        pairs: I,
    ) -> Result<Expr<'i>, BakugoParsingError<'i>> {
        PRATT_PARSER
            .map_primary(|primary| match primary.as_rule() {
                // TODO: check if this match is needed
                Rule::IntLit | Rule::Ident | Rule::FunctionCall => Expr::parse(primary),
                _ => Err(BakugoParsingError::new(
                    primary.as_span(),
                    "got a non expression".to_owned(),
                    BakugoParsingErrorKind::InternalError,
                )),
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
                    _ => {
                        return Err(BakugoParsingError::new(
                            op.as_span(),
                            "got a non operator".to_owned(),
                            BakugoParsingErrorKind::InternalError,
                        ))
                    }
                };
                Ok(Expr::BinaryExpr {
                    left: Box::new(left?),
                    op,
                    right: Box::new(right?),
                })
            })
            .map_prefix(|op, right| {
                let op = match op.as_rule() {
                    Rule::UnaryMinus => UnaryOp::Minus,
                    Rule::UnaryStar => UnaryOp::Star,
                    Rule::UnaryPlus => UnaryOp::Plus,
                    Rule::UnaryAnd => UnaryOp::And,
                    Rule::UnaryNot => UnaryOp::Not,
                    _ => {
                        return Err(BakugoParsingError::new(
                            op.as_span(),
                            "got a non operator".to_owned(),
                            BakugoParsingErrorKind::InternalError,
                        ))
                    }
                };
                Ok(Expr::UnaryExpr {
                    op,
                    operand: Box::new(right?),
                })
            })
            .map_postfix(|left, op| match op.as_rule() {
                Rule::Arguments => {
                    // TODO: refactor this, does not look good
                    let mut inner = op.into_inner();
                    let is_kind = inner
                        .peek()
                        .map(|pair| pair.as_rule() == Rule::Type)
                        .unwrap_or(false);

                    let kind = if is_kind {
                        let kind = Some(Kind::parse(inner.next().unwrap())?);
                        // this next skips comma
                        inner.next();
                        kind
                    } else {
                        None
                    };
                    Ok(Expr::FunctionCall {
                        function: Box::new(left?),
                        args: Args {
                            kind,
                            args: inner
                                .next()
                                .map(Self::parse_expr_list)
                                .transpose()?
                                .unwrap_or_else(Vec::new),
                        },
                    })
                }
                _ => {
                    return Err(BakugoParsingError::new(
                        op.as_span(),
                        "got a non arguments".to_owned(),
                        BakugoParsingErrorKind::InternalError,
                    ))
                }
            })
            .parse(pairs)
    }

    fn parse_expr_list(pairs: Pair<'i, Rule>) -> Result<Vec<Expr<'i>>, BakugoParsingError> {
        // having this inside the 'map_postfix()' of parse_expr() leads to a compile
        // time recursion limit error.
        let mut inner = pairs.into_inner();
        let mut args = vec![];
        while inner.peek().is_some() {
            let inner_expr = inner
                .by_ref()
                .take_while(|pair| pair.as_rule() != Rule::Comma);
            args.push(Expr::parse_expr(inner_expr)?);
        }
        Ok(args)
    }
}

#[derive(Debug)]
pub enum Statement<'i> {
    Return, // TODO: add expression list
    Expression(Expr<'i>),
}

impl<'i> Node<'i> for Statement<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Result<Self, BakugoParsingError> {
        match pair.as_rule() {
            Rule::ExpressionStmt => {
                let expr = Expr::parse_expr(pair.into_inner())?;
                Ok(Statement::Expression(expr))
            }
            Rule::ReturnStmt => Ok(Statement::Return),
            _ => {
                return Err(BakugoParsingError::new(
                    pair.as_span(),
                    "got a statement".to_owned(),
                    BakugoParsingErrorKind::InternalError,
                ))
            }
        }
    }
}
