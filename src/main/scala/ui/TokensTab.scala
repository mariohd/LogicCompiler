package ui

import java.awt.BorderLayout
import javax.swing.{JScrollPane, JTable, JPanel}
import javax.swing.table.AbstractTableModel

import structure._

/**
  * Created by MarioDiniz on 08/09/16.
  */
class TokensTab(tokens: List[Token]) extends JPanel {

  start
  draw

  private def start = {
    setLayout(new BorderLayout())
  }

  private def draw: Unit = {

    val dataModel = new AbstractTableModel() {
      private val _tokens = tokens

      override def getColumnName(i: Int): String = {
        i match {
          case 0 => "Category"
          case 1 => "Value"
        }
      }

      def getColumnCount: Int = 2
      def getRowCount: Int = _tokens.size
      def getValueAt(row: Int, col: Int): Object = {
        col match {
          case 0 => _tokens.lift(row).get.category
          case 1 => _tokens.lift(row).get.value
        }
      }
    }

    val table = new JTable(dataModel)
    add(new JScrollPane(table))
  }
}