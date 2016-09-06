package compiler

import structure.ASTDefinition._
import structure.TokenCategories._
import structure._

import scala.collection.mutable.ListBuffer

/**
  * Created by MarioDiniz on 31/08/16.
  */


object Parser {

  def tokenizer(s: String): List[Token] = {
    var currentCharPosition = 0
    var tokens = ListBuffer[Token]()
    val Pattern = "([A-Z])".r

    while(currentCharPosition < s.length) {
      val character = s.charAt(currentCharPosition)

      character match {
        case '(' => tokens += new Token(OpenParenthesis, "(", currentCharPosition)
        case ')' => tokens += new Token(CloseParenthesis, ")", currentCharPosition)
        case '^' => tokens += new Token(AndOperator, "^", currentCharPosition)
        case 'v' => tokens += new Token(OrOperator, "v", currentCharPosition)
        case '~' => tokens += new Token(NotOperator, "~", currentCharPosition)
        case '-' if s.charAt(currentCharPosition + 1).equals('>') =>
          tokens += new Token(ImpliesOperator, "->", currentCharPosition); currentCharPosition += 1
        case ' ' => ()
        case Pattern(c) => tokens += new Token(Premise, character.toString, currentCharPosition)
      }

      currentCharPosition += 1
    }
    tokens.toList
  }

  def isValidExpression(tokens: List[Token], str: String): Boolean = {
    var expected = List[TokenCategory](NotOperator, Premise, OpenParenthesis)
    val finalTokens = List[TokenCategory](CloseParenthesis, Premise)

    def traverse(): Boolean = {
      val tokenIterator = tokens.toSeq.iterator
      var parenthesisCount = 0
      var token: Token = null

      while(tokenIterator.hasNext) {
        token = tokenIterator.next()

        if (! expected.contains(token.category)) {
          syntaxError(token, str)
        }

        token.category match {
          case OpenParenthesis =>
            parenthesisCount += 1
            expected = token.nextValidTokens.get
          case CloseParenthesis =>
            parenthesisCount -= 1
            expected = token.nextValidTokens.get
          case _ => expected = token.nextValidTokens.get
        }
      }

      if (! finalTokens.contains(token.category) ) {
        syntaxError(token, str)
      }

      if (parenthesisCount != 0) {
        throw new NoSuchElementException(
          s"Missing ${ if (parenthesisCount > 0) CloseParenthesis else OpenParenthesis }.")
      }

      true
    }

    traverse()
  }

  def generateAST(tokens: List[Token]): AST = {
    var token: Token = null
    var ast: AST = null
    var close = 0
    val tokenIterator = tokens.toSeq.iterator

    def walk(c: Boolean): AST = {
      token = tokenIterator.next()

      ast = token.category match {
        case Premise => new NodeProp(token)
        case NotOperator => new ASTUnary(token).addChild(walk(true))
        case AndOperator | OrOperator | ImpliesOperator => {
          val bTree = ASTBinary(token)
          bTree.addChildLeft(ast)
          bTree.addChildRight(walk(true))
        }
        case CloseParenthesis => {
          close -= 1
          ast
        }
        case OpenParenthesis => {
          var loopExpression: AST = null
          close += 1
          def loop(): AST = {
            if (close != 0) {
              loopExpression = walk(c)
              loop()
            } else {
              loopExpression
            }
          }

          loop()
        }
      }

      if (!c && tokenIterator.hasNext) walk(false)

      ast
    }

    walk(false)
  }

  private  def syntaxError(token: Token, s: String): Unit = {
     throw new scala.UnsupportedOperationException(
      s"Unexpected Token found at ${token.position} \n" +
      s"input   : $s \n" +
      s"          ${' '.toString * (token.position - 1)}\u2934\n" +
      s"found   : $token \n" +
      s"expected: ${token.nextValidTokens.get}")
  }
}
