package ui

import java.awt.{Color, Component, BorderLayout}
import javax.swing._
import javax.swing.table.{DefaultTableCellRenderer, DefaultTableModel, AbstractTableModel}

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
  private val finalResults = evaluateCombinations.toMap

  start
  draw

  private def draw = {

    val columnNames = premises.map(_.value.asInstanceOf[Object]).toArray
    val dataModel = new CustomDataModel(columnNames :+ "Expression Result")
    finalResults.foreach(r => {
      val rowValues = r._1.values.foldLeft(Array[Object]())((array, element) => array :+ element.toString.toUpperCase.asInstanceOf[Object]) :+ r._2.toString.toUpperCase.asInstanceOf[Object]
      dataModel.addRow(rowValues)
    })

    val table = new JTable(dataModel)
    table.setRowSelectionAllowed(false)
    for (i <- 0 to columnNames.size )
      table.getColumnModel.getColumn(i).setCellRenderer(new ColoredColumnCellRender)
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

  class ColoredColumnCellRender extends DefaultTableCellRenderer {
    setHorizontalAlignment(SwingConstants.CENTER)

    override  def getTableCellRendererComponent(table: JTable, value: Object, isSelected: Boolean, hasFocus: Boolean, row: Int, col: Int): Component = {
      val labelRenderer = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col).asInstanceOf[JLabel]
      val tm = table.getModel.asInstanceOf[DefaultTableModel]

      val cellValue = tm.getValueAt(row, col).toString.toBoolean == true
      if (cellValue){
        labelRenderer.setBackground(new Color(197, 249, 176))
      } else {
        labelRenderer.setBackground(new Color(251, 162, 156))
      }

      labelRenderer
    }
  }

  class CustomDataModel(columnsName: Array[Object]) extends DefaultTableModel(columnsName, 0) {

    override def isCellEditable(row: Int, col: Int): Boolean = {
      false
    }
  }

}
