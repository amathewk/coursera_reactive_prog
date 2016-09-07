package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    val ne = namedExpressions map { case (name, signal:Signal[Expr]) =>
      if (hasCycle(signal(), namedExpressions, List(name))) (name, Signal(Double.NaN))
      else (name,Signal(eval(signal(), namedExpressions)))
    }


    ne
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => eval (getReferenceExpr(name,references), references)
      case Plus(a, b) => eval (a,references) + eval (b, references)
      case Minus(a, b) => eval (a,references) - eval (b, references)
      case Times(a, b) => eval (a,references) * eval (b, references)
      case Divide(a, b) => eval (a,references) / eval (b, references)
    }
  }

  def hasCycle(expr: Expr, references: Map[String, Signal[Expr]], cumReferences: List[String]): Boolean = {
    expr match {
      case Literal(v) => false
      case Ref(name) => (cumReferences contains name) || hasCycle(getReferenceExpr(name, references), references, name::cumReferences)
      case Plus(a, b) => hasCycle(a, references, cumReferences) || hasCycle(b, references, cumReferences)
      case Minus(a, b) => hasCycle(a, references, cumReferences) || hasCycle(b, references, cumReferences)
      case Times(a, b) => hasCycle(a, references, cumReferences) || hasCycle(b, references, cumReferences)
      case Divide(a, b) => hasCycle(a, references, cumReferences) || hasCycle(b, references, cumReferences)
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
