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

  def solveIt(ast: AST, values: Map[Token, Boolean]): (Boolean, AST) = {

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
    val result = finder(ast)
    (result, ast )
  }
}
