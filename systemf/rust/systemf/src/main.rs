//! A simple implementation of System F.

/// A type expression.
/// System F differentiates types (TypeExpr) from terms (Expr).
#[derive(Debug)]
enum TypeExpr {
  /// A type variable.
  Id(String),
  /// A function type (a -> b).
  Arrow(Box<TypeExpr>, Box<TypeExpr>),
  /// A forall, (forall a b).
  ForAll(String, Box<TypeExpr>),
}

/// A term expression.
#[derive(Debug)]
enum Expr {
  /// A term variable.
  Id(String),
  /// Applying one term to another.
  Apply(Box<Expr>, Box<Expr>),
  /// A lambda.
  Abstract(String, TypeExpr, Box<Expr>),
  /// Applying a term to a type.
  TypeApply(Box<Expr>, TypeExpr),
  /// A type lambda.
  TypeAbstract(String, Box<Expr>),
}

extern crate combine;
use combine::{between, many, many1, parser, skip_many, token, try};
use combine::{Parser, ParseResult, State, Stream};
use combine::char::{letter, space, spaces, string};

/// A parse function for TypeExpr.
/// This is needed because combine can't define recursive parsers without it.
fn type_expr<I>(input: I) -> ParseResult<TypeExpr, I>
  where I: Stream<Item = char>
{
  let identifier = || many1(letter()).skip(spaces()).map(TypeExpr::Id);
  let arrow = string("->").skip(spaces());
  fn to_arrow(l: Vec<TypeExpr>, e: TypeExpr) -> TypeExpr {
    let reviter = l.into_iter().rev();
    reviter.fold(e, |e, prev| TypeExpr::Arrow(Box::new(prev), Box::new(e)))
  }
  let mut arrow = (many(try(identifier().skip(arrow))), identifier())
      .map(|(l, e)| to_arrow(l, e));
  arrow.parse_stream(input)
}

/// A parse function for Expr.
/// This is needed because combine can't define recursive parsers without it.
fn expr<I>(input: I) -> ParseResult<Expr, I>
  where I: Stream<Item = char>
{
  fn identifier<I>(input: I) -> ParseResult<Expr, I>
    where I: Stream<Item = char>
  {
    many1(letter()).map(Expr::Id).parse_stream(input)
  }
  fn abstraction<I>(input: I) -> ParseResult<Expr, I>
    where I: Stream<Item = char>
  {
    let lam = token('\\').skip(spaces());
    let abs = (lam, many1(letter()).skip(spaces()), parser(type_expr::<I>), parser(expr::<I>));
    let mut abs_expr = abs.map(|(_, var, typ, exp)| Expr::Abstract(var, typ, Box::new(exp)));
    abs_expr.parse_stream(input)
  }
  let typ = between(token('[').skip(spaces()), token(']').skip(spaces()), parser(type_expr::<I>));
  fn subexpr<I>(input: I) -> ParseResult<Expr, I>
    where I: Stream<Item = char>
  {
    parser(identifier).or(parser(abstraction)).parse_stream(input)
  }
  enum ExprOrType {
    E(Expr),
    T(TypeExpr),
  }
  fn to_apply(e: Expr, l: Vec<ExprOrType>) -> Expr {
    l.into_iter().fold(e, |result, next| {
      match next {
        ExprOrType::E(next) => Expr::Apply(Box::new(result), Box::new(next)),
        ExprOrType::T(next) => Expr::TypeApply(Box::new(result), next),
      }
    })
  }
  let e_or_t = parser(subexpr).map(ExprOrType::E).or(typ.map(ExprOrType::T));
  let white = skip_many(space());
  let mut apply = (parser(subexpr), many(white.with(e_or_t)))
      .map(|(e, l)| to_apply(e, l));
  apply.parse_stream(input)
}

// Parses its parameter, printing the result.
fn parse(input: &str) -> () {
  match parser(expr).parse(State::new(input)) {
    Ok((e, _)) => println!("{:?}", e),
    Err(e) => println!("{}", e),
  }
}

fn main() {
  parse("\\f a -> b i [a -> b] f x");
}
