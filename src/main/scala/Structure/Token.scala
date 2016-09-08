package structure

/**
  * Created by MarioDiniz on 31/08/16.
  */
import structure.TokenCategories._

case class Token(val t: TokenCategory, val v: String, val p: Int = 0) {
  val category = t
  var value = v
  val position = p

  override def toString: String = {
    s"Token[$category, value: $value]"
  }

  def nextValidTokens: Option[List[TokenCategory]] = {
    category match {
      case OpenParenthesis => Some(List(NotOperator, OpenParenthesis, Premise))
      case CloseParenthesis => Some(List(OrOperator, AndOperator, ImpliesOperator, CloseParenthesis))
      case Premise => Some(List(OrOperator, AndOperator, ImpliesOperator, CloseParenthesis))
      case OrOperator | AndOperator | ImpliesOperator | NotOperator => Some(List(OpenParenthesis, Premise, NotOperator))
      case _ => None
    }
  }

  override def equals(o: Any) = o match {
    case that: Token => (this.category == that.category) && (this.value == that.value)
    case _ => false
  }
}
