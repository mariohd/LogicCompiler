package compiler

import structure.ASTDefinition.{NodeProp, ASTUnary, ASTBinary, AST}
import structure.Token
import structure.TokenCategories._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by MarioDiniz on 10/11/16.
  */
object Proof {

  def removeImplications(ast: AST, tokens: List[Token]) : AST = {
    var newTokens = ListBuffer[Token]()

    def scan (t: Token) : Unit = {
      if (t.category.equals(ImpliesOperator)) {
        var before = newTokens.remove(newTokens.length - 1)
        newTokens += new Token(NotOperator, "\u223C")
        newTokens += before
        newTokens += new Token(OrOperator, "\u22C1")
      } else {
        newTokens += t;
      }
    }

    tokens.foreach(scan)

    Parser.generateAST(newTokens.toList)

  }
}
