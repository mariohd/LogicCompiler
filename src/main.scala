/**
  * Created by MarioDiniz on 31/08/16.
  */
object main {
  def main(args: Array[String]): Unit = {
    val expression = "~(PvQ) -> (~P^~Q)" // deMorgan
    val tokens = parsing.tokenizer(expression)
    println(parsing.isValidExpression(tokens))
  }
}


