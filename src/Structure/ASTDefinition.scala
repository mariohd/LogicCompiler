package Structure

import NodeCategories._
/**
  * Created by MarioDiniz on 04/09/16.
  */
object ASTDefinition {

  class AST() {}

  case class NodeProp(val t: Token) extends AST {
    val category = PropNode
    val token = t

    override def toString: String = s"[token: $token, category: $category]"
  }

  case class ASTUnary(val t: Token, c: AST = null ) extends AST {
    val category = UnaryNode
    val token = t
    var child: AST = null

    def addChild(c: AST): AST = {
      this.child = c
      this
    }

    override def toString: String = s"[token: $token, category: $category, child: $child]"
  }

  case class ASTBinary(val t: Token) extends AST {
    val category = BinaryNode
    val token = t

    var child_left, child_right: AST = null

    def addChildLeft(c: AST): AST = {
      this.child_left = c
      this
    }

    def addChildRight(c: AST): AST = {
      this.child_right = c
      this
    }

    override def toString: String = s"[token: $token, category: $category, child-left: $child_left, child-right: $child_right]"
  }
}
