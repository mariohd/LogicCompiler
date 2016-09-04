package Structure

/**
  * Created by MarioDiniz on 31/08/16.
  */
import Structure.TokenCategories._

case class Token(val t: TokenCategory, val v: String, val p: Int = 0) {
  val category = t
  val value  = v
  val position = p

  override def toString(): String = {
    s"[category: ${category}, value: ${value}]"
  }

  def nextValidTokens(): Option[List[TokenCategory]] = {
    this.category match {
      case OpenParenthesis => Some(List(NotOperator, OpenParenthesis, Premise))
      case CloseParenthesis => Some(List(OrOperator, AndOperator, ImpliesOperator))
      case Premise => Some(List(OrOperator, AndOperator, ImpliesOperator, CloseParenthesis))
      case OrOperator | AndOperator | ImpliesOperator | NotOperator => Some(List(OpenParenthesis, Premise, NotOperator))
      case _ => None
    }
  }
}
