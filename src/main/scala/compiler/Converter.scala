package compiler

import java.io._
import java.nio.charset.Charset
import java.nio.file.{Paths, Files}
import java.util.Scanner
import javax.script.{Invocable, ScriptEngineManager}

import scala.io.Source

/**
  * Created by MarioDiniz on 16/12/16.
  */
object Converter {
  def toCNF(premise: String) : String = {
    val tokens = Parser.tokenizer(premise)
    val ast = Parser.generateAST(tokens)
    val CNF = formulaToCNF(ast.toStr)
    return CNFtoFormula(CNF)
  }

  private def formulaToCNF(str: String) : String = {
    val pw = new PrintWriter(new File("sentences.txt" ))
    pw.write("1\n")
    pw.write(str)
    pw.close

    val p = Runtime.getRuntime().exec("python CNFConverter -i sentences.txt")
    p.waitFor()

    val filename = "sentences_CNF.txt"
    return Source.fromFile(filename).getLines().mkString
  }

  private def CNFtoFormula(cnf: String): String = {

    val manager = new ScriptEngineManager()
    val engine = manager.getEngineByName("JavaScript")
    val jsFile = new File("CNFToFormulaConverter.js")

    def putJsInString(file: File, encoding: Charset): String = {
      val encoded = Files.readAllBytes(Paths.get(file.getAbsolutePath()))
      return new String(encoded, encoding);
    }

    def execute(fn: String, parameters: Array[Object]): Object = {
      val script = putJsInString(jsFile, Charset.defaultCharset())
      engine.eval(script)

      val inv = engine.asInstanceOf[Invocable]

      return inv.invokeFunction(fn , parameters)
    }

    var x = execute("discover", Array(cnf))

    return x.toString
  }
}
