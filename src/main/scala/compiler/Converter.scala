package compiler

import java.io._
import java.util.Scanner

import scala.io.Source

/**
  * Created by MarioDiniz on 16/12/16.
  */
object Converter {
  def toCNF(premise: String) : String = {
    val tokens = Parser.tokenizer(premise)
    val ast = Parser.generateAST(tokens)
    val CNF = generate(ast.toStr)
    return CNF
  }

  private def generate(str: String) : String = {
    val pw = new PrintWriter(new File("sentences.txt" ))
    pw.write("1\n")
    pw.write(str)
    pw.close

    val p = Runtime.getRuntime().exec("python CNFConverter -i sentences.txt")
    p.waitFor()

    val filename = "sentences_CNF.txt"
    return Source.fromFile(filename).getLines().mkString
  }
}
