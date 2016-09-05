/**
  * Created by MarioDiniz on 31/08/16.
  */
object main {
  def main(args: Array[String]): Unit = {
    val expression = "~(XvY) -> (~P^~Q)" // deMorgan
    //val expression = "(Q^P)" // deMorgan
    val tokens = parsing.tokenizer(expression)
    if (parsing.isValidExpression(tokens, expression))
      println(parsing.generateAST(tokens))
  }
}


