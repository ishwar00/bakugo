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
    #[error("Error")]
    Error,
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

impl<'i> Node<'i> for SourceFile<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Result<Self, BakugoParsingError> {
        check_rule(&pair, Rule::SourceFile, "top level decl")?;

        let pairs = pair.into_inner();
        let mut top_level = vec![];
        for pair in pairs {
            // TODO: find a better way to do this
            // see: https://github.com/pest-parser/pest/issues/327
            // We need semicolons in grammar to get good errors
            // (Ex: "expecting Semicolon") but we ignore it during AST construction.
            if matches!(pair.as_rule(), Rule::Semicolon | Rule::EOI) {
                continue;
            }
            let inner = pair.into_inner().next().unwrap();
            match inner.as_rule() {
                Rule::FunctionDecl => top_level.push(TopLevelDecl::FnDecl(FnDecl::parse(inner)?)),
                Rule::Declaration => {
                    Decl::parse(inner.into_inner().next().unwrap(), |decl| {
                        top_level.push(match decl {
                            Decl::Var(v) => TopLevelDecl::VarDecl(v),
                            Decl::Const(c) => TopLevelDecl::ConstDecl(c),
                            Decl::Kind(k) => TopLevelDecl::KindDecl(k),
                        })
                    })?;
                }
                _ => todo!("non decls"),
            };
        }
        Ok(SourceFile { top_level })
    }
}

#[derive(Debug)]
pub enum TopLevelDecl<'i> {
    FnDecl(FnDecl<'i>),
    VarDecl(VarConstDecl<'i>),
    ConstDecl(VarConstDecl<'i>),
    KindDecl(KindDecl<'i>),
}

#[derive(Debug)]
pub struct FnDecl<'i> {
    pub name: Ident<'i>,
    pub params: Vec<FnParameterDecl<'i>>,
    pub result_kind: Option<Kind<'i>>,
    pub body: StatementList<'i>,
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

        let body = StatementList::parse(body_pair)?;

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
                invalid_rule => {
                    return Err(BakugoParsingError::new(
                        first.as_span(),
                        format!(
                            "Expected only types and identifiers in parameters. Got {:?}",
                            invalid_rule
                        ),
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

#[derive(Debug, Clone)]
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
                    invalid_rule => {
                        return Err(BakugoParsingError::new(
                            span,
                            format!(
                                "expected type but got something else. got: {:?}.",
                                invalid_rule
                            ),
                            BakugoParsingErrorKind::InternalError,
                        ))
                    }
                }
                Ok(Self::Tuple { kinds, span })
            }
            invalid_rule => {
                return Err(BakugoParsingError::new(
                    inner.as_span(),
                    format!(
                        "expected type but got something else. got: {:?}.",
                        invalid_rule
                    ),
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
    Eq,
    NotEq,
    Le,
    Lt,
    Ge,
    Gt,
}

#[derive(Debug)]
pub enum Expr<'i> {
    Identifier {
        value: &'i str,
        span: Span<'i>,
    },
    Integer {
        value: i64, // TODO: other int types?
        span: Span<'i>,
    },
    Float {
        value: f64, // TODO: other float types?
        span: Span<'i>,
    },
    String {
        value: String,
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
    TupleExpr(Vec<Expr<'i>>),
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
            Rule::FloatLit => Ok(Expr::Float {
                value: pair.as_str().parse().unwrap(),
                span: pair.as_span(),
            }),
            Rule::RawStringLit => Ok(Expr::String {
                value: pair.as_str().trim_matches('`').to_owned(),
                span: pair.as_span(),
            }),
            Rule::InterpretedStringLit => Ok(Expr::String {
                // TODO: parse escape characters
                value: pair.as_str().trim_matches('"').to_owned(),
                span: pair.as_span(),
            }),
            Rule::TupleExpr => {
                let expr_list = Expr::parse_expr_list(pair)?;

                Ok(Expr::TupleExpr(expr_list))
            }
            Rule::FunctionCall => Ok(Self::parse_expr(pair.into_inner())?),
            invalid_rule => Err(BakugoParsingError::new(
                pair.as_span(),
                format!("expected an expression. got {:?}", invalid_rule),
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
                Rule::FloatLit
                | Rule::IntLit
                | Rule::Ident
                | Rule::InterpretedStringLit
                | Rule::RawStringLit
                | Rule::FunctionCall
                | Rule::TupleExpr => Expr::parse(primary),
                invalid_rule => Err(BakugoParsingError::new(
                    primary.as_span(),
                    format!("expected an expresion. got {:?}", invalid_rule),
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
                    Rule::Eq => BinaryOp::Eq,
                    Rule::NotEq => BinaryOp::NotEq,
                    Rule::Le => BinaryOp::Le,
                    Rule::Lt => BinaryOp::Lt,
                    Rule::Ge => BinaryOp::Ge,
                    Rule::Gt => BinaryOp::Gt,
                    _ => {
                        return Err(BakugoParsingError::new(
                            op.as_span(),
                            // TODO: use a general function to result this kinds of errors.
                            // also include the Rule that was found here in the error message.
                            "got a non operator".to_owned(),
                            BakugoParsingErrorKind::InternalError,
                        ));
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
                                .unwrap_or_default(),
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
    Return(Option<Vec<Expr<'i>>>),
    Declaration(Decl<'i>),
    Expression(Expr<'i>),
    Assignment { left: Expr<'i>, right: Expr<'i> },
    IfStmt(IfStmt<'i>),
}

#[derive(Debug)]
pub struct IfStmt<'i> {
    pub ifs: Vec<(Expr<'i>, StatementList<'i>)>,
    pub els: Option<StatementList<'i>>,
}

#[derive(Debug)]
pub struct StatementList<'i>(Vec<Statement<'i>>);

#[derive(Debug)]
pub enum Decl<'i> {
    Var(VarConstDecl<'i>),
    Const(VarConstDecl<'i>),
    Kind(KindDecl<'i>),
}

#[derive(Debug)]
pub struct VarConstDecl<'i> {
    pub name: Ident<'i>,
    pub kind: Option<Kind<'i>>,
    pub expr: Expr<'i>,
}

#[derive(Debug)]
pub struct KindDecl<'i> {
    pub name: Ident<'i>,
    pub kind: Kind<'i>,
}

impl<'i> Decl<'i> {
    fn parse(
        pair: Pair<'i, Rule>,
        mut new_decl: impl FnMut(Decl<'i>),
    ) -> Result<(), BakugoParsingError<'i>> {
        match pair.as_rule() {
            Rule::VarDecl | Rule::ConstDecl => {
                let is_var = pair.as_rule() == Rule::VarDecl;
                let var_const_specs = pair.into_inner().filter(|p| p.as_rule() != Rule::Semicolon);

                for var_const_spec in var_const_specs {
                    let spec_span = var_const_spec.as_span();

                    if !matches!(var_const_spec.as_rule(), Rule::VarSpec | Rule::ConstSpec) {
                        return Err(BakugoParsingError::new(
                            var_const_spec.as_span(),
                            format!(
                                "expected a var or const decl. got {:?}.",
                                var_const_spec.as_rule()
                            ),
                            BakugoParsingErrorKind::InternalError,
                        ));
                    }

                    let mut var_spec_inner = var_const_spec.into_inner();
                    let idents = var_spec_inner.next().unwrap();

                    let maybe_kind = var_spec_inner.next().unwrap();
                    let exprs;
                    let kind = if maybe_kind.as_rule() == Rule::Type {
                        exprs = var_spec_inner.next().unwrap();
                        Some(Kind::parse(maybe_kind)?)
                    } else {
                        exprs = maybe_kind;
                        None
                    };

                    let idents = idents
                        .into_inner()
                        .map(Ident::parse)
                        .collect::<Result<Vec<Ident>, _>>()?;
                    let exprs = Expr::parse_expr_list(exprs)?;

                    if idents.len() != exprs.len() {
                        return Err(BakugoParsingError::new(
                            spec_span,
                            format!(
                                "Number of expressions ({}) does not match number of identifers ({})",
                                exprs.len(),
                                idents.len()
                            ),
                            BakugoParsingErrorKind::Error,
                        ));
                    }

                    for (ident, expr) in idents.into_iter().zip(exprs.into_iter()) {
                        let decl = VarConstDecl {
                            name: ident,
                            kind: kind.clone(),
                            expr,
                        };
                        new_decl(if is_var {
                            Decl::Var(decl)
                        } else {
                            Decl::Const(decl)
                        });
                    }
                }
            }

            Rule::TypeDecl => {
                let type_defs = pair
                    .into_inner()
                    .filter(|pair| pair.as_rule() != Rule::Semicolon);

                for type_def in type_defs {
                    let mut inner = type_def.into_inner();
                    let identifier = inner.next().unwrap();
                    let kind = inner.next().unwrap();

                    new_decl(Decl::Kind(KindDecl {
                        name: Ident::parse(identifier)?,
                        kind: Kind::parse(kind)?,
                    }));
                }
            }

            invalid_rule => {
                return Err(BakugoParsingError::new(
                    pair.as_span(),
                    format!("expected a declaration. got {:?}", invalid_rule),
                    BakugoParsingErrorKind::InternalError,
                ))
            }
        }

        Ok(())
    }
}

impl<'i> Node<'i> for StatementList<'i> {
    fn parse(pair: Pair<'i, Rule>) -> Result<Self, BakugoParsingError> {
        match pair.as_rule() {
            Rule::StatementList => {
                let mut parsed_stmts = Vec::new();

                for pair in pair.into_inner() {
                    match pair.as_rule() {
                        Rule::ExpressionStmt => {
                            let expr = Expr::parse_expr(pair.into_inner())?;
                            parsed_stmts.push(Statement::Expression(expr))
                        }

                        Rule::ReturnStmt => match pair.into_inner().next() {
                            Some(expr_list) => parsed_stmts
                                .push(Statement::Return(Some(Expr::parse_expr_list(expr_list)?))),
                            None => parsed_stmts.push(Statement::Return(None)),
                        },

                        Rule::Assignment => {
                            let span = pair.as_span();
                            let mut expr_iter = pair.into_inner();
                            let left_expr_list = expr_iter.next().unwrap();
                            let right_expr_list = expr_iter.next().unwrap();

                            let left_exprs = Expr::parse_expr_list(left_expr_list)?;

                            let right_exprs = Expr::parse_expr_list(right_expr_list)?;

                            if left_exprs.len() != right_exprs.len() {
                                return Err(BakugoParsingError::new(
                                    span,
                                    format!("Number of expressions on left side ({}) does not match the right side ({})", left_exprs.len(), right_exprs.len()),
                                    BakugoParsingErrorKind::Error,
                                ));
                            }

                            for (left, right) in left_exprs.into_iter().zip(right_exprs.into_iter())
                            {
                                parsed_stmts.push(Statement::Assignment { left, right })
                            }
                        }

                        Rule::IfStmt => {
                            let mut ifs = Vec::new();
                            let els = Self::parse_ifstmt(&mut ifs, pair)?;
                            parsed_stmts.push(Statement::IfStmt(IfStmt { ifs, els }));
                        }

                        Rule::Semicolon => {}

                        Rule::Declaration => {
                            Decl::parse(pair.into_inner().next().unwrap(), |decl| {
                                parsed_stmts.push(Statement::Declaration(decl))
                            })?
                        }

                        invalid_rule => {
                            return Err(BakugoParsingError::new(
                                pair.as_span(),
                                format!("expected a statement. got {:?}.", invalid_rule),
                                BakugoParsingErrorKind::InternalError,
                            ))
                        }
                    }
                }

                Ok(Self(parsed_stmts))
            }
            invalid_rule => {
                return Err(BakugoParsingError::new(
                    pair.as_span(),
                    format!("expected a statement list. got {:?}.", invalid_rule),
                    BakugoParsingErrorKind::InternalError,
                ))
            }
        }
    }
}

impl<'i> StatementList<'i> {
    fn parse_ifstmt(
        v: &mut Vec<(Expr<'i>, StatementList<'i>)>,
        pair: Pair<'i, Rule>,
    ) -> Result<Option<StatementList<'i>>, BakugoParsingError<'i>> {
        let mut inner = pair.into_inner();
        let cond = Expr::parse_expr(inner.next().unwrap().into_inner())?;
        let block = StatementList::parse(inner.next().unwrap())?;

        v.push((cond, block));

        match inner.next() {
            Some(p) if p.as_rule() == Rule::IfStmt => Self::parse_ifstmt(v, p),
            Some(p) if p.as_rule() == Rule::StatementList => Ok(Some(StatementList::parse(p)?)),
            None => Ok(None),
            Some(_) => unreachable!("if stmt can only have ifstmt or statementlist"),
        }
    }
}
