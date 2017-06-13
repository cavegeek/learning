#[derive(Debug)]
enum TypeExpr {
  Id(String),
  Arrow(Box<TypeExpr>, Box<TypeExpr>),
  ForAll(String, Box<TypeExpr>)
}

#[derive(Debug)]
enum Expr {
  Id(String),
  Apply(Box<Expr>, Box<Expr>),
  Abstract(String, TypeExpr, Box<Expr>),
  TypeApply(Box<Expr>, TypeExpr),
  TypeAbstract(String, Box<Expr>)
}

fn main() {
  println!("{:?}", Expr::TypeAbstract(String::from("a"), Box::new(Expr::Abstract(String::from("x"), TypeExpr::Id(String::from("a")), Box::new(Expr::Id(String::from("x")))))));
}
