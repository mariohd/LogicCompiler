import Structure.TokenCategories._
import Structure._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by MarioDiniz on 31/08/16.
  */


object parsing {

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

  def isValidExpression(tokens: List[Token]): Boolean = {
    var expected = List[TokenCategory](NotOperator, Premise, OpenParenthesis)
    val finalTokens = List[TokenCategory](CloseParenthesis, Premise)

    def traverse(): Boolean = {
      val tokenIterator = tokens.toSeq.iterator
      var parenthesisCount = 0
      var token: Token = null

      while(tokenIterator.hasNext) {
        token = tokenIterator.next()

        if (! expected.contains(token.category)) {
          throw new UnsupportedOperationException(
            s"Unexpected Token found at ${token.position} \n" +
            s"found   : $token \n" +
            s"expected: ${ token.nextValidTokens().get }")
        }

        token match {
          case Token(OpenParenthesis, _, _) =>
            parenthesisCount += 1
            expected = token.nextValidTokens().get
          case Token(CloseParenthesis, _, _) =>
            parenthesisCount -= 1
            expected = token.nextValidTokens().get
          case Token(_, _, _) => expected = token.nextValidTokens().get
        }
      }

      if (! finalTokens.contains(token.category) ) {
        throw new UnsupportedOperationException(
          s"Unexpected ending Token found at ${token.position} \n" +
          s"found   : $token \n" +
          s"expected: ${ token.nextValidTokens().get }")
      }

      if (parenthesisCount != 0) {
        println()
        throw new UnsupportedOperationException(
          s"Missing ${ if (parenthesisCount > 0) CloseParenthesis else OpenParenthesis }.")
      }

      true
    }

    traverse()
  }

  // OTHER IMPLEMENTATION FOR THE SAME PROBLEM

  def recursiveTokenizer(s: String): List[Token] = {

    @tailrec
    def loop(str: String, tokens: List[Token] = List()): List[Token] = {
      if(str.length > 0) {
        val character = str.head
        val next = str.substring(1)

        character match {
          case '(' => loop(next, tokens :+ new Token(OpenParenthesis, "(", s.indexOf(str)))
          case ')' => loop(next, tokens :+ new Token(CloseParenthesis, ")", s.indexOf(str)))
          case '^' => loop(next, tokens :+ new Token(AndOperator, "^", s.indexOf(str)))
          case 'v' => loop(next, tokens :+ new Token(OrOperator, "v", s.indexOf(str)))
          case '~' => loop(next, tokens :+ new Token(NotOperator, "~", s.indexOf(str)))
          case '-' if next.head.equals('>') => loop(next.substring(1), tokens :+ new Token(ImpliesOperator, "->", s.indexOf(str)))
          case ' ' => loop(next, tokens)
          case _ => loop(next, tokens :+ new Token(Premise, character.toString, s.indexOf(str)))
        }
      } else {
        tokens
      }
    }
    loop(s)
  }

  def isValidExpression2(initialTokens: List[Token]): Boolean = {
    var parenthesisCount = 0

    def init(tokens: List[Token]): Boolean = {
      if (tokens.isEmpty) return false

      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
      }
    }

    def premise(tokens: List[Token]): Boolean = {
      if (tokens.isEmpty) if (parenthesisCount == 0) return true else return false

      tokens.head match {
        case Token(AndOperator, _, _) => andOperator(tokens.tail)
        case Token(OrOperator, _, _) => orOperator(tokens.tail)
        case Token(ImpliesOperator, _, _) => impliesOperator(tokens.tail)
        case Token(CloseParenthesis, _, _) => closeParenthesis(tokens.tail);
        case _ => fail(tokens.head)
      }
    }

    def notOperator(tokens: List[Token]): Boolean = {
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail)
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => fail(tokens.head)
      }
    }

    def openParenthesis(tokens: List[Token]): Boolean = {
      parenthesisCount += 1
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail)
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => fail(tokens.head)
      }
    }

    def andOperator(tokens: List[Token]): Boolean = {
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail);
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => fail(tokens.head)
      }
    }

    def orOperator(tokens: List[Token]): Boolean = {
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail);
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => fail(tokens.head)
      }
    }

    def impliesOperator(tokens: List[Token]): Boolean = {
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail);
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => fail(tokens.head)
      }
    }

    def closeParenthesis(tokens: List[Token]): Boolean = {
      parenthesisCount -= 1
      if (tokens.isEmpty) if (parenthesisCount == 0) return true else return false

      tokens.head match {
        case Token(AndOperator, _, _) => andOperator(tokens.tail)
        case Token(OrOperator, _, _) => orOperator(tokens.tail)
        case Token(ImpliesOperator, _, _) => impliesOperator(tokens.tail)
        case _ => fail(tokens.head)
      }
    }

    def fail(token: Token) : Boolean = {
      throw new UnsupportedOperationException(
        s"Unexpected Token found at ${token.position} \n" +
        s"found   : $token \n" +
        s"expected: ${ token.nextValidTokens().get }")
    }

    init(initialTokens)
  }
}
