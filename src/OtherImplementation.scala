import Structure.Token
import Structure.TokenCategories._

import scala.annotation.tailrec

/**
  * Created by MarioDiniz on 04/09/16.
  */
object OtherImplementation {
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

  def isValidExpression(initialTokens: List[Token]): Boolean = {
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
        case _ => syntaxError(tokens.head)
      }
    }

    def notOperator(tokens: List[Token]): Boolean = {
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail)
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => syntaxError(tokens.head)
      }
    }

    def openParenthesis(tokens: List[Token]): Boolean = {
      parenthesisCount += 1
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail)
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => syntaxError(tokens.head)
      }
    }

    def andOperator(tokens: List[Token]): Boolean = {
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail);
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => syntaxError(tokens.head)
      }
    }

    def orOperator(tokens: List[Token]): Boolean = {
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail);
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => syntaxError(tokens.head)
      }
    }

    def impliesOperator(tokens: List[Token]): Boolean = {
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail);
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => syntaxError(tokens.head)
      }
    }

    def closeParenthesis(tokens: List[Token]): Boolean = {
      parenthesisCount -= 1
      if (tokens.isEmpty) if (parenthesisCount == 0) return true else return false

      tokens.head match {
        case Token(AndOperator, _, _) => andOperator(tokens.tail)
        case Token(OrOperator, _, _) => orOperator(tokens.tail)
        case Token(ImpliesOperator, _, _) => impliesOperator(tokens.tail)
        case _ => syntaxError(tokens.head)
      }
    }

    init(initialTokens)
  }

  private  def syntaxError(token: Token): Nothing = {
    throw new scala.UnsupportedOperationException(
      s"Unexpected Token found at ${token.position} \n" +
        s"found   : $token \n" +
        s"expected: ${token.nextValidTokens.get}")
  }
}
