package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (name, signal) => (name, Signal[Double](eval(signal(), namedExpressions)))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def helper(expr: Expr, evaluated: Set[String]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(n) => if (evaluated.contains(n)) Double.NaN else helper(getReferenceExpr(n, references), evaluated + n)
        case Plus(a, b) => helper(a, evaluated) + helper(b, evaluated)
        case Minus(a, b) => helper(a, evaluated) - helper(b, evaluated)
        case Times(a, b) => helper(a, evaluated) * helper(b, evaluated)
        case Divide(a, b) => helper(a, evaluated) / helper(b, evaluated)
      }
    }
    helper(expr, Set[String]())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
