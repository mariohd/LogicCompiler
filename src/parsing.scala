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
    val noWhiteSpaces = s filter (_ != ' ')

    while(currentChar < noWhiteSpaces.length) {
      val character = noWhiteSpaces.charAt(currentChar)

      character match {
        case '(' => tokens += new Token(TokenCategories.OpenParenthesis, "(")
        case ')' => tokens += new Token(TokenCategories.CloseParenthesis, ")")
        case '^' => tokens += new Token(TokenCategories.AndOperator, "^")
        case 'v' => tokens += new Token(TokenCategories.OrOperator, "v")
        case '~' => tokens += new Token(TokenCategories.NotOperator, "~")
        case '-' if noWhiteSpaces.charAt(currentChar + 1).equals('>') =>
          tokens += new Token(TokenCategories.ImpliesOperator, "->"); currentChar += 1
        case _ => tokens += new Token(TokenCategories.Premise, character.toString)
      }

      currentChar += 1
    }
    tokens.toList
  }

  def recursiveTokenizer(s: String): List[Token] = {

    @tailrec
    def loop(str: String, tokens: List[Token] = List()): List[Token] = {
      if(str.length > 0) {
        val character = str.head
        val next = str.substring(1)

        character match {
          case '(' => loop(next, tokens :+ new Token(TokenCategories.OpenParenthesis, "("))
          case ')' => loop(next, tokens :+ new Token(TokenCategories.CloseParenthesis, ")"))
          case '^' => loop(next, tokens :+ new Token(TokenCategories.AndOperator, "^"))
          case 'v' => loop(next, tokens :+ new Token(TokenCategories.OrOperator, "v"))
          case '~' => loop(next, tokens :+ new Token(TokenCategories.NotOperator, "~"))
          case '-' if next.head.equals('>') => loop(next.substring(1), tokens :+ new Token(TokenCategories.ImpliesOperator, "->"))
          case _ => loop(next, tokens :+ new Token(TokenCategories.Premise, character.toString))
        }
      } else {
        tokens
      }
    }
    loop(s filter (_ != ' '))
  }

  def isValidExpression(tokens: List[Token]): Boolean = {
    var expected = ListBuffer[TokenCategories.TokenCategory](TokenCategories.NotOperator, TokenCategories.Premise, TokenCategories.OpenParenthesis)
    val finalTokens = ListBuffer[TokenCategories.TokenCategory](TokenCategories.CloseParenthesis, TokenCategories.Premise)

    def traverse(): Boolean = {
      val tokenInterator = tokens.toSeq.iterator
      var parenthesisCount = 0
      var token = Token(TokenCategories.Premise, "")

      while(tokenInterator.hasNext) {
        token = tokenInterator.next()

        if (! expected.exists(_ == token.category)) {
          println(s"Unexpected ${token} found. " +
                  s"Expected one of the following: ${expected}" )
          return false
        } else {
          expected.clear()
        }

        token match {
          case Token(TokenCategories.Premise, p) => {
            expected += (TokenCategories.AndOperator, TokenCategories.OrOperator, TokenCategories.ImpliesOperator , TokenCategories.CloseParenthesis)
          }
          case Token(TokenCategories.NotOperator, _) => {
            expected += (TokenCategories.Premise, TokenCategories.OpenParenthesis, TokenCategories.AndOperator)
          }
          case Token(TokenCategories.OrOperator, _) => {
            expected += (TokenCategories.Premise, TokenCategories.OpenParenthesis, TokenCategories.NotOperator)
          }
          case Token(TokenCategories.AndOperator, _) => {
            expected += (TokenCategories.Premise, TokenCategories.OpenParenthesis, TokenCategories.NotOperator)
          }
          case Token(TokenCategories.ImpliesOperator, _) => {
            expected += (TokenCategories.Premise, TokenCategories.OpenParenthesis, TokenCategories.NotOperator)
          }
          case Token(TokenCategories.OpenParenthesis, _) => {
            parenthesisCount += 1
            expected += (TokenCategories.Premise, TokenCategories.OpenParenthesis, TokenCategories.NotOperator)
          }
          case Token(TokenCategories.CloseParenthesis, _) => {
            parenthesisCount -= 1
            expected += (TokenCategories.OrOperator, TokenCategories.AndOperator, TokenCategories.ImpliesOperator)
          }
        }
      }

      if (! finalTokens.exists(_ == token.category) ) {
        println(s"Unexpected END Token for: $token");
        return false;
      }

      if (parenthesisCount != 0) {
        println(s"Missing ${ if (parenthesisCount > 0)  Token(TokenCategories.CloseParenthesis, ")") else Token(TokenCategories.OpenParenthesis, "(")}.");
        return false;
      }

      return true;
    }

    traverse()
  }

}
