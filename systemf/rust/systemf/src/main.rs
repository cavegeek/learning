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
use combine::{many, Parser, State};
use combine::char::letter;

// Parses its parameter, returning the corresponding expression or None.
fn parse(input: &str) -> Option<Expr> {
  match many(letter()).parse(input) {
    Ok((id, "")) => Some(Expr::Id(id)),
    _ => None,
  }
}

fn main() {
  println!("{:?}", parse("varname"));
}
