package ui

import java.awt.BorderLayout
import javax.swing.{JScrollPane, JTable, JPanel}
import javax.swing.table.AbstractTableModel

import compiler.Solver
import structure.ASTDefinition.AST
import structure.Token
import structure.TokenCategories._

import scala.annotation.tailrec

/**
  * Created by MarioDiniz on 08/09/16.
  */
class TruthTableTab(ast: AST, tokens: List[Token]) extends JPanel {
  private val premises = tokens.filter(_.category == Premise).toSet.toList
  private val resultSet = gen(premises.size)
  private val mappedTokens = resultSet.map(rs => (premises zip rs).toMap)

  start
  draw

  private def draw = {

    val dataModel = new AbstractTableModel() {
      private val finalResults = evaluateCombinations.toMap

      override def getColumnName(i: Int): String = {
        i match {
          case x if (premises.size > x) => premises(x).value
          case _ => "Expression Result"
        }
      }

      def getColumnCount: Int = (premises.size + 1)

      def getRowCount: Int = Math.pow(2, premises.size).toInt

      def getValueAt(row: Int, col: Int): Object = {
        val premisesRow = mappedTokens.toList(row)
        println(row, col)
        col match {
          case x if (premises.size > x) => premisesRow.values.toList(x).toString.toUpperCase.charAt(0).toString
          case y if (premises.size == y) => finalResults.get(premisesRow).get.toString.toUpperCase
          case _ => "???"
        }
      }
    }

    val table = new JTable(dataModel)
    add(new JScrollPane(table))

  }

  private def evaluateCombinations = {
    val iterator = mappedTokens.iterator

    @tailrec
    def loop(acc: List[(Map[Token, Boolean],Boolean)] = List()): List[(Map[Token, Boolean],Boolean)] = {
      if (iterator.hasNext) {
        val m = iterator.next
        val (result, _) = Solver.solveIt(ast, m)
        loop((m, result) :: acc)
      } else {
        acc
      }

    }

    loop()
  }

  private def start = {
    setLayout(new BorderLayout())
  }

  private def gen(int: Int, acc: List[Boolean] = Nil): Set[List[Boolean]] = {
    val values = Set(true, false)
    if (int > 1) {
      values.flatMap { p => gen(int - 1, p :: acc)}
    }
    else {
      values.map(q => q :: acc)
    }
  }
}
