import Structure.TokenCategories._
import Structure._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by MarioDiniz on 31/08/16.
  */


object parsing {

  def tokenizer(s: String): List[Token] = {
    var currentChar = 0
    var tokens = ListBuffer[Token]()

    while(currentChar < s.length) {
      val character = s.charAt(currentChar)

      character match {
        case '(' => tokens += new Token(OpenParenthesis, "(")
        case ')' => tokens += new Token(CloseParenthesis, ")")
        case '^' => tokens += new Token(AndOperator, "^")
        case 'v' => tokens += new Token(OrOperator, "v")
        case '~' => tokens += new Token(NotOperator, "~")
        case '-' if s.charAt(currentChar + 1).equals('>') =>
          tokens += new Token(ImpliesOperator, "->"); currentChar += 1
        case ' ' => ()
        case _ => tokens += new Token(Premise, character.toString)
      }

      currentChar += 1
    }
    tokens.toList
  }

  def isValidExpression(tokens: List[Token]): Boolean = {
    var expected = ListBuffer[TokenCategory](NotOperator, Premise, OpenParenthesis)
    val finalTokens = ListBuffer[TokenCategory](CloseParenthesis, Premise)

    def traverse(): Boolean = {
      val tokenInterator = tokens.toSeq.iterator
      var parenthesisCount = 0
      var token = Token(Premise, "")

      while(tokenInterator.hasNext) {
        token = tokenInterator.next()

        if (! expected.contains(token.category)) {
          println(s"Unexpected $token found. " +
                  s"Expected one of the following: $expected" )
          return false
        } else {
          expected.clear()
        }

        token match {
          case Token(Premise, p, _) => expected += (AndOperator, OrOperator, ImpliesOperator , CloseParenthesis)

          case Token(NotOperator, _, _) => expected += (Premise, OpenParenthesis)
          case Token(OrOperator, _, _) => expected += (Premise, OpenParenthesis, NotOperator)
          case Token(AndOperator, _, _) => expected += (Premise, OpenParenthesis, NotOperator)
          case Token(ImpliesOperator, _, _) => expected += (Premise, OpenParenthesis, NotOperator)
          case Token(OpenParenthesis, _, _) =>
            parenthesisCount += 1
            expected += (Premise, OpenParenthesis, NotOperator)
          case Token(CloseParenthesis, _, _) =>
            parenthesisCount -= 1
            expected += (OrOperator, AndOperator, ImpliesOperator)
        }
      }

      if (! finalTokens.contains(token.category) ) {
        println(s"Unexpected END Token for: $token")
        return false
      }

      if (parenthesisCount != 0) {
        println(s"Missing ${ if (parenthesisCount > 0)  Token(CloseParenthesis, ")") else Token(OpenParenthesis, "(")}.")
        return false
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
          case '(' => loop(next, tokens :+ new Token(OpenParenthesis, "("))
          case ')' => loop(next, tokens :+ new Token(CloseParenthesis, ")"))
          case '^' => loop(next, tokens :+ new Token(AndOperator, "^"))
          case 'v' => loop(next, tokens :+ new Token(OrOperator, "v"))
          case '~' => loop(next, tokens :+ new Token(NotOperator, "~"))
          case '-' if next.head.equals('>') => loop(next.substring(1), tokens :+ new Token(ImpliesOperator, "->"))
          case ' ' => loop(next, tokens)
          case _ => loop(next, tokens :+ new Token(Premise, character.toString))
        }
      } else {
        tokens
      }
    }
    loop(s)
  }

  def isValidExpression2(initialTokens: List[Token]): Boolean = {
    var parenthisCount = 0

    def init(tokens: List[Token]): Boolean = {
      if (tokens.isEmpty) return false

      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
      }
    }

    def premise(tokens: List[Token]): Boolean = {
      if (tokens.isEmpty) if (parenthisCount == 0) return true else return false

      tokens.head match {
        case Token(AndOperator, _, _) => andOperator(tokens.tail)
        case Token(OrOperator, _, _) => orOperator(tokens.tail)
        case Token(ImpliesOperator, _, _) => implieOperator(tokens.tail)
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
      parenthisCount += 1
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

    def implieOperator(tokens: List[Token]): Boolean = {
      tokens.head match {
        case Token(Premise, _, _) => premise(tokens.tail);
        case Token(OpenParenthesis, _, _) => openParenthesis(tokens.tail)
        case Token(NotOperator, _, _) => notOperator(tokens.tail)
        case _ => fail(tokens.head)
      }
    }

    def closeParenthesis(tokens: List[Token]): Boolean = {
      parenthisCount -= 1
      if (tokens.isEmpty) if (parenthisCount == 0) return true else return false

      tokens.head match {
        case Token(AndOperator, _, _) => andOperator(tokens.tail)
        case Token(OrOperator, _, _) => orOperator(tokens.tail)
        case Token(ImpliesOperator, _, _) => implieOperator(tokens.tail)
        case _ => fail(tokens.head)
      }
    }

    def fail(token: Token) : Boolean = {
      throw new UnsupportedOperationException(s"Unexpected Token found at ${token.position} \n" +
        s"found   : $token \n" +
        s"expected: ${ token.nextValidTokens().get }")
    }

    init(initialTokens)
  }
}
