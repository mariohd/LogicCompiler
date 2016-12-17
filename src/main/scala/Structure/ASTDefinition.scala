package structure

import NodeCategories._
/**
  * Created by MarioDiniz on 04/09/16.
  */
object ASTDefinition {

  private val RED = "#fb8e88"
  private val GREEN = "#b1f99c"
  private val DEFAULT = "#c3d9ff"

  abstract class AST {
    def toStr: String
  }

  case class NodeProp(val t: Token) extends AST {
    val category = PropNode
    val token = t
    var resultValue: Option[Boolean] = None

    override def toString: String = s"[token: $token, category: $category]"

    def nodeColor(): String = {
      resultValue match {
        case Some(true) => GREEN
        case Some(false) => RED
        case _ => DEFAULT
      }
    }

    override def toStr: String = s"'${t.value}'"
  }

  case class ASTUnary(val t: Token) extends AST {
    val category = UnaryNode
    val token = t
    var child: AST = null
    var resultValue: Option[Boolean] = None

    override def toStr: String = {
      return s"['${token.category.toString.toLowerCase.replace("operator", "") }', ${this.child.toStr}]"
    }

    def addChild(c: AST): AST = {
      this.child = c
      this
    }

    override def toString: String = s"[token: $token, category: $category, child: $child]"

    def nodeColor(): String = {
      resultValue match {
        case Some(true) => GREEN
        case Some(false) => RED
        case _ => DEFAULT
      }
    }
  }

  case class ASTBinary(val t: Token) extends AST {
    val category = BinaryNode
    val token = t
    var child_left, child_right: AST = null
    var resultValue: Option[Boolean] = None

    def addChildLeft(c: AST): AST = {
      this.child_left = c
      this
    }

    def addChildRight(c: AST): AST = {
      this.child_right = c
      this
    }

    def nodeColor(): String = {
      resultValue match {
        case Some(true) => GREEN
        case Some(false) => RED
        case _ => DEFAULT
      }
    }

    def toStr: String = {
      return s"['${token.category.toString.toLowerCase.replace("operator", "") }', ${this.child_left.toStr}, ${this.child_right.toStr}]"
    }

    override def toString: String = s"[token: $token, category: $category, child-left: $child_left, child-right: $child_right]"
  }
}
