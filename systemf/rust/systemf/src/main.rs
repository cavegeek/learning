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

fn main() {
  println!("{:?}", Expr::TypeAbstract(String::from("a"), Box::new(Expr::Abstract(String::from("x"), TypeExpr::Id(String::from("a")), Box::new(Expr::Id(String::from("x")))))));
}
