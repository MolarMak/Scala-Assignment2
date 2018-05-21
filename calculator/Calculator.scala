package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions
      .mapValues(it => Signal { eval(it(), namedExpressions) })
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(i) => i
      case Ref(i) => eval(getReferenceExpr(i, references), references.filterKeys(_ != i))
      case Plus(i1, i2) => eval(i1, references) + eval(i2, references)
      case Minus(i1, i2) => eval(i1, references) - eval(i2, references)
      case Times(i1, i2) => eval(i1, references) * eval(i2, references)
      case Divide(i1, i2) => eval(i1, references) / eval(i2, references)
    }
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
