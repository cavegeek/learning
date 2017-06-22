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
use combine::{choice, many1, parser, sep_by, skip_many1};
use combine::{Parser, ParseResult, State, Stream};
use combine::char::{letter, space, spaces, string};

/// A parse function for TypeExpr.
/// This is needed because combine can't define recursive parsers without it.
fn type_expr<I>(input: I) -> ParseResult<TypeExpr, I>
  where I: Stream<Item = char>
{
  let identifier = || many1(letter()).skip(spaces()).map(TypeExpr::Id);
  let arrow = || string("->").skip(spaces());
  let arrow_expr = sep_by(identifier(), arrow());
  fn to_arrow(e: TypeExpr, l: Vec<TypeExpr>) -> TypeExpr {
    let mut result = e;
    for next in l {
      result = TypeExpr::Arrow(Box::new(result), Box::new(next));
    }
    result
  }
  let mut arrow = (identifier(), arrow(), arrow_expr).map(|(e, _, l)| to_arrow(e, l));
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
    let lam = string("\\").skip(spaces());
    let abs = (lam, many1(letter()).skip(spaces()), parser(type_expr::<I>), parser(expr::<I>));
    let mut abs_expr = abs.map(|(_, var, typ, exp)| Expr::Abstract(var, typ, Box::new(exp)));
    abs_expr.parse_stream(input)
  }
  let subexpr = || parser(identifier).or(parser(abstraction));
  let white = || skip_many1(space());
  let subexpr_seq = sep_by(subexpr(), white());
  fn to_apply(e: Expr, l: Vec<Expr>) -> Expr {
    let mut result = e;
    for next in l {
      result = Expr::Apply(Box::new(result), Box::new(next));
    }
    result
  }
  let mut apply = (subexpr(), white(), subexpr_seq).map(|(e, _, l)| to_apply(e, l));
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
  parse("\\x ta -> tb x y ");
}
