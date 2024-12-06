use winnow::Parser;

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone)]
pub struct Tagged<T> {
    pub u: T,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum UExpr {
    Atom(Atom),
    Apply(Box<Expr>, Vec<Expr>),
    Compose(Vec<Expr>),
    AbstractCompose(Vec<Var>, Vec<Expr>),
}
pub type Expr = Tagged<UExpr>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum Atom {
    Var(Var),
    Integer(i64),
    Float(f64),
    String(String),
}

type PInp<'s> = winnow::stream::Located<&'s str>;
type PRes<T> = winnow::PResult<T>;

struct Whitespace;
impl winnow::stream::ContainsToken<char> for Whitespace {
    fn contains_token(&self, c: char) -> bool {
        (' ', '\t', '\r', '\n').contains_token(c)
    }
}

struct VarToken;
impl winnow::stream::ContainsToken<char> for VarToken {
    fn contains_token(&self, c: char) -> bool {
        !Whitespace.contains_token(c)
            && !('(', ')', '[', ']', '{', '}').contains_token(c)
    }
}

fn consume_whitespace(inp: &mut PInp) -> PRes<()> {
    winnow::token::take_while(0.., Whitespace).void().parse_next(inp)
}

pub fn parse_var(inp: &mut PInp) -> PRes<Var> {
    consume_whitespace.parse_next(inp)?;
    let nm = winnow::token::take_while(1.., VarToken).parse_next(inp)?;
    consume_whitespace.parse_next(inp)?;
    winnow::PResult::Ok(Var {
        name: nm.to_owned(),
    })
}

pub fn parse_atom(inp: &mut PInp) -> PRes<Atom> {
    winnow::combinator::alt((
        winnow::ascii::dec_int
            .map(|i| Atom::Integer(i)),
        winnow::combinator::delimited(
            '"',
            winnow::token::take_till(0.., '"'),
            '"',
        ).map(|s: &str| Atom::String(s.to_owned())),
        parse_var.map(Atom::Var),
    )).parse_next(inp)
}

pub fn parse_apply(inp: &mut PInp) -> PRes<UExpr> {
    "(".parse_next(inp)?;
    let f = parse_expr.parse_next(inp)?;
    let xs = winnow::combinator::repeat(0.., parse_expr).parse_next(inp)?;
    ")".parse_next(inp)?;
    winnow::PResult::Ok(UExpr::Apply(Box::new(f), xs))
}

pub fn parse_compose(inp: &mut PInp) -> PRes<UExpr> {
    "[".parse_next(inp)?;
    let fs = winnow::combinator::repeat(0.., parse_expr).parse_next(inp)?;
    "]".parse_next(inp)?;
    winnow::PResult::Ok(UExpr::Compose(fs))
}

pub fn parse_abstract_compose(inp: &mut PInp) -> PRes<UExpr> {
    "[{".parse_next(inp)?;
    let vs = winnow::combinator::repeat(0.., parse_var).parse_next(inp)?;
    "}".parse_next(inp)?;
    let fs = winnow::combinator::repeat(0.., parse_expr).parse_next(inp)?;
    "]".parse_next(inp)?;
    winnow::PResult::Ok(UExpr::AbstractCompose(vs, fs))
}

pub fn parse_expr(inp: &mut PInp) -> PRes<Expr> {
    consume_whitespace.parse_next(inp)?;
    let (u, span) = winnow::combinator::alt((
        parse_atom.map(UExpr::Atom),
        parse_apply,
        parse_compose,
        parse_abstract_compose,
    )).with_span().parse_next(inp)?;
    consume_whitespace.parse_next(inp)?;
    winnow::PResult::Ok(
        Tagged { u, span }
    )
}
