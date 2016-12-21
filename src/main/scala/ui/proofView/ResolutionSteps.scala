package ui.proofView

import java.awt._
import javax.swing._
import javax.swing.border.EmptyBorder
import javax.swing.table.{TableColumn, DefaultTableCellRenderer, AbstractTableModel}

import scala.collection.mutable.ListBuffer

/**
  * Created by MarioDiniz on 21/12/16.
  */
class ResolutionSteps(premises: String, theorem: String) extends JFrame {

  private val dataModel = new ResolutionDataModel
  private val table = new JTable(dataModel)
  private val label = new JLabel(s"$premises \u22AC $theorem", SwingConstants.CENTER)
  private val statusBar = new JPanel(new FlowLayout(FlowLayout.LEFT))
  private val resultLabel = new JLabel(s"<html>Theorem " +
                                        s"<font color='red''>NOT PROVED</font>" +
                                       s"</html>")
  config
  start

  private def start = {
    val screenSize = Toolkit.getDefaultToolkit.getScreenSize
    setSize(new Dimension(
      (screenSize.getWidth * .60).toInt,
      (screenSize.getHeight * .60).toInt))
    setLocationRelativeTo(null)
    setVisible(true)
  }
  private def config = {
    setLayout(new BorderLayout())
    setTitle(s"Resolution View")
    add(new JScrollPane(table), BorderLayout.CENTER)

    table.setAutoResizeMode(JTable.AUTO_RESIZE_NEXT_COLUMN)
    val indexColumn = table.getColumnModel.getColumn(0)
    indexColumn.setMaxWidth(50)

    val centerRenderer = new DefaultTableCellRenderer
    centerRenderer.setHorizontalAlignment( SwingConstants.CENTER )
    indexColumn.setCellRenderer(centerRenderer)

    label.setFont(new Font("Serif", Font.PLAIN, 20))
    label.setBorder(new EmptyBorder(15, 15, 15, 15))
    add(label, BorderLayout.NORTH)

    add(statusBar, BorderLayout.SOUTH)
    statusBar.add(resultLabel)
  }

  def isProved = {
    label.setText(s"$premises \u22A2 $theorem")
    resultLabel.setText(s"<html>Theorem " +
      s"<font color='green''>PROVED</font>" +
      s"</html>")
    repaint()
    validate()
  }

  def receiveStep(elements: (String, String, String)): Unit = {
    dataModel.addResolutionStep(elements)
  }

  class ResolutionDataModel extends AbstractTableModel {
    val steps = ListBuffer[(String, String, String)]()

    override def getRowCount: Int = steps.size

    override def getColumnCount: Int = 4

    override def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
      columnIndex match {
        case 0 => (rowIndex + 1).toString
        case 1 => sanitize(steps.lift(rowIndex).get._1)
        case 2 => sanitize(steps.lift(rowIndex).get._2)
        case 3 => sanitize(steps.lift(rowIndex).get._3)
      }
    }

    override def getColumnName(i: Int): String = {
      i match {
        case 0 => "Step"
        case 1 => "Clause A"
        case 2 => "Clause B"
        case 3 => "Results in"
      }
    }

    def addResolutionStep(elements: (String, String, String)) = {
      steps += elements
      this.fireTableDataChanged
    }

    private def sanitize(s: String): String =
      s.replaceAll("->", "\u2192").replaceAll(" ^", " \u2227").replaceAll("v", "\u22C1")
  }

}
