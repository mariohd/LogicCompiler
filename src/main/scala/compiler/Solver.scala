package compiler

import structure.ASTDefinition._
import structure.Token
import structure.TokenCategories._

/**
  * Created by MarioDiniz on 05/09/16.
  */
object Solver {

  def solveIt(ast: AST, values: Map[Token, Boolean]): (Boolean, AST) = {

    def resetAst(node: AST): Unit = {
      node match {
        case n: NodeProp => n.resultValue = None
        case n: ASTUnary => {
          n.resultValue = None
          resetAst(n.child)
        }
        case n: ASTBinary => {
          n.resultValue = None
          resetAst(n.child_left)
          resetAst(n.child_right)
        }
      }
    }

    def finder(node: AST): Boolean = {
      node match {
        case n: NodeProp => n.resultValue = Some(values.get(n.token).get); n.resultValue.get
        case n: ASTUnary => unaryOperation(n)
        case n: ASTBinary => binaryOperation(n)
      }
    }

    def unaryOperation(ast: ASTUnary): Boolean = {
      ast.token.category match {
        case NotOperator => ast.resultValue = Some(!(finder(ast.child))); ast.resultValue.get
      }
    }

    def binaryOperation(ast: ASTBinary): Boolean = {
      ast.token.category match {
        case AndOperator => ast.resultValue = Some(finder(ast.child_left) && finder(ast.child_right)); ast.resultValue.get
        case OrOperator => ast.resultValue = Some(finder(ast.child_left) || finder(ast.child_right)); ast.resultValue.get
        case ImpliesOperator => {
          val f = finder(ast.child_left)
          if (f) {
            ast.resultValue = Some(f && finder(ast.child_right))
            ast.resultValue.get
          } else {
            ast.resultValue = Some(true)
            ast.resultValue.get
          }
        }
      }
    }

    resetAst(ast)
    val result = finder(ast)
    (result, ast )
  }
}
