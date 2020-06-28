
trait Expr {
}

case class MyNumber(n: Int) extends Expr
case class MySum(e1: Expr, e2: Expr) extends Expr
case class MyProd(e1: Expr, e2: Expr) extends Expr
case class Var(name: String) extends Expr

def eval(e: Expr): Int = e match {
  case MyNumber(n) => n
  case MySum(e1, e2) => eval(e1) + eval(e2)
  case MyProd(e1, e2) => eval(e1) * eval(e2)
}
def show(e: Expr): String = e match {
  case MyNumber(x) => x.toString
  case Var(x) => x
  case MySum(e1, e2) => show(e1) + " + " + show(e2)
  case MyProd(e1: MySum, e2: MySum) => "(" + show(e1) + ") * (" + show(e2) + ")"
  case MyProd(e1: MySum, e2) => "(" + show(e1) + ") * " + show(e2)
  case MyProd(e1, e2: MySum) => show(e1) + " * (" + show(e2) + ")"
  case MyProd(e1, e2) => show(e1) + " * " + show(e2)
}

show {MySum(MyProd(MyNumber(2), Var("x")), Var("y"))}

show {MyProd(MySum(MyNumber(2), Var("x")), Var("y"))}

show {MyProd(MySum(MyNumber(2), Var("x")), MySum(MyNumber(3), Var("y")))}