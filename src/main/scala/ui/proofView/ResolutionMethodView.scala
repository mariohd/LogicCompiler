package ui.proofView

import java.awt.event._
import java.awt._
import javax.swing._

import ui.premiseView.{PremiseView, ResultPanel}

/**
  * Created by MarioDiniz on 16/12/16.
  */
class ResolutionMethodView extends JFrame {
  private val inputPanel = new JPanel
  private val premiseTablePanel = new JPanel(new BorderLayout)
  private val dataModel = new PremiseDataModel

  config
  configInput
  start

  private def start = {
    setSize(new Dimension(800, 600))
    setLocationRelativeTo(null)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setVisible(true)
  }

  private def config = {
    setLayout(new BorderLayout())
    add(inputPanel, BorderLayout.NORTH)
    add(premiseTablePanel, BorderLayout.CENTER)
    setIconImage(new ImageIcon("resources/icon.png").getImage)
    setTitle(s"Resolution Method proof")
  }

  private def configInput = {
    val inputField = new JTextField(50)
    val addPremise = new JButton("Add")
    inputPanel.add(inputField)
    inputPanel.add(addPremise)

    addPremise.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val premise = inputField.getText;
        dataModel.addPremise(premise)
        getContentPane.repaint()
        getContentPane.validate()
      }
    })

    val table = new JTable(dataModel)
    premiseTablePanel.add(new JScrollPane(table), BorderLayout.CENTER)

    table.getColumn("Viewer").setCellRenderer(new ButtonRenderer)
    table.getColumn("Remove").setCellRenderer(new ButtonRenderer)

    table.addMouseListener(new MouseAdapter {
      override def mouseClicked(evt: MouseEvent) {
        val row = table.rowAtPoint(evt.getPoint)
        val col = table.columnAtPoint(evt.getPoint)
        col match {
          case 2 => new PremiseView(dataModel.premises.lift(row).get.premise)
          case 3 => dataModel.removePremise(row)
          case _ => {}
        }
      }
    })

    table.addMouseMotionListener(new MouseMotionAdapter {
      override def mouseMoved (evt: MouseEvent) {
        val col = table.columnAtPoint(evt.getPoint)
        if (col >= 2) {
          table.setCursor (Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))
        } else {
          table.setCursor (Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR))
        }
      }
    })
  }
}
