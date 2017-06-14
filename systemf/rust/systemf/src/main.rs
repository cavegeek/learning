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
  ForAll(String, Box<TypeExpr>)
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
  TypeAbstract(String, Box<Expr>)
}

extern crate combine;
use combine::{many1, parser, sep_by, skip_many1};
use combine::{Parser, ParseResult, Stream};
use combine::char::{letter, space};

/// A parse function for Expr.
/// This is needed because combine can't define recursive parsers without it.
fn expr<I>(input: I) -> ParseResult<Expr, I>
  where I: Stream<Item=char>
{
  let identifier = || many1(letter()).map(Expr::Id);
  let subexpr = || identifier();
  let white = || skip_many1(space());
  let subexpr_seq = sep_by(subexpr(), white());
  fn to_apply(e : Expr, l : Vec<Expr>) -> Expr {
    let mut result = e;
    for next in l {
      result = Expr::Apply(Box::new(result), Box::new(next));
    }
    result
  }
  let mut apply = (subexpr(), white(), subexpr_seq).map(|(e,_,l)| to_apply(e,l));
  apply.parse_stream(input)
}

// Parses its parameter, returning the corresponding expression or None.
fn parse(input: &str) -> Option<Expr> {
  match parser(expr).parse(input) {
    Ok((e, "")) => Some(e),
    _ => None,
  }
}

fn main() {
  println!("{:?}", parse("f x y"));
}
