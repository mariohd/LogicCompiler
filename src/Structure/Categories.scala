package Structure

/**
  * Created by MarioDiniz on 31/08/16.
  */
object TokenCategories extends Enumeration {
  type TokenCategory = Value
  val NotOperator, OpenParenthesis, CloseParenthesis, Premise, OrOperator, AndOperator, ImpliesOperator = Value
}

object NodeCategories extends Enumeration {
  type NodeCategory = Value
  val ParentNode, PropNode, UnaryNode, BinaryNode = Value
}
