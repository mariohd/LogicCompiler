package compiler

import structure.ASTDefinition._
import structure.Token
import structure.TokenCategories._

/**
  * Created by MarioDiniz on 05/09/16.
  */
object Solver {

  def main(args: Array[String]): Unit = {
    val tokens =  Parser.tokenizer("~(PvQ) -> (~P^~Q)")
    val premises = tokens.filter(_.category == Premise)
    val values = premises.map(t =>
      if (t.value == "P") (t, true) else (t, true)
    ).toMap
    val ast = Parser.generateAST(tokens)
    println(solveIt(ast, values))
  }

  def solveIt(ast: AST, values: Map[Token, Boolean]): Boolean = {

    def finder(node: AST): Boolean = {
      node match {
        case n: NodeProp => values.get(n.token).get
        case n: ASTUnary => unaryOperation(n)
        case n: ASTBinary => binaryOperation(n)
      }
    }

    def unaryOperation(ast: ASTUnary): Boolean = {
      ast.token.category match {
        case NotOperator => !(finder(ast.child))
      }
    }

    def binaryOperation(ast: ASTBinary): Boolean = {
      ast.token.category match {
        case AndOperator => finder(ast.child_left) && finder(ast.child_right)
        case OrOperator => finder(ast.child_left) || finder(ast.child_right)
        case ImpliesOperator => {
          val f = finder(ast.child_left)
          if (f) {
             f && finder(ast.child_right)
          } else {
            true
          }
        }
      }
    }

    finder(ast)
  }
}
