package compiler

import java.io.{IOException, BufferedReader, InputStreamReader}

import structure.ASTDefinition.{NodeProp, ASTUnary, ASTBinary, AST}
import structure.Token
import structure.TokenCategories._

import scala.collection.mutable.ListBuffer

/**
  * Created by MarioDiniz on 10/11/16.
  */
object Proof {

  def normalize(tokens: List[Token]): AST = {
    val removedImplications = removeImplications(tokens)
    val intenalizedNegations = internalizeNegations(removedImplications)

    Parser.generateAST(intenalizedNegations)
  }

  private def removeImplications(tokens: List[Token]) : List[Token] = {
    var newTokens = ListBuffer[Token]()

    def scan(t: Token) : Unit = {
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

    newTokens.toList
  }

  private def internalizeNegations(tokens: List[Token]): List[Token] = {
    var newTokens = ListBuffer[Token]()
    var s:String = "";

    try {
      var p = Runtime.getRuntime().exec("python CNFConverter -i sentences.txt");

      var stdInput = new BufferedReader(new
          InputStreamReader(p.getInputStream()));

      var stdError = new BufferedReader(new
          InputStreamReader(p.getErrorStream()));

      System.out.println("Here is the standard error of the command (if any):\n");
      while (s != null) {
        s = stdInput.readLine();
        System.out.println(s);
      }

      System.out.println("Here is the standard error of the command (if any):\n");
      while (s != null) {
        s = stdError.readLine();
        System.out.println(s);
      }
    }
    catch  {
      case e: IOException => {
        System.out.println("exception happened - here's what I know: ");
        e.printStackTrace();
        System.exit(-1);
      }
    }

    tokens.toList
  }
}
