package ui.proofView

import java.awt.event._
import java.awt._
import javax.swing._
import javax.swing.text.DocumentFilter.FilterBypass
import javax.swing.text.{DocumentFilter, AttributeSet, AbstractDocument}

import ui.premiseView.{PremiseView, ResultPanel}

/**
  * Created by MarioDiniz on 16/12/16.
  */
class ResolutionMethodView extends JFrame {
  private val inputPanel = new JPanel
  private val theoremPanel = new JPanel
  private val premiseTablePanel = new JPanel(new BorderLayout)
  private val dataModel = new PremiseDataModel

  config
  configInput
  start

  private def start = {
    val screenSize = Toolkit.getDefaultToolkit.getScreenSize
    setSize(new Dimension(
      (screenSize.getWidth * .75).toInt,
      (screenSize.getHeight * .75).toInt))
    setLocationRelativeTo(null)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setVisible(true)
  }

  private def config = {
    setLayout(new BorderLayout())
    val topPanel = new JPanel(new BorderLayout)
    topPanel.add(inputPanel, BorderLayout.NORTH)
    topPanel.add(theoremPanel, BorderLayout.SOUTH)
    add(topPanel, BorderLayout.NORTH)
    add(premiseTablePanel, BorderLayout.CENTER)
    setIconImage(new ImageIcon("resources/icon.png").getImage)
    setTitle(s"Resolution Method proof")
  }

  private def configInput = {
    val inputField = new JTextField(50)
    val premiseLabel = new JLabel("Premises")
    val addPremise = new JButton("Add")
    addPremise.setPreferredSize(new Dimension(80, 35))

    inputPanel.add(premiseLabel)
    inputPanel.add(inputField)
    inputPanel.add(addPremise)

    addPremise.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val premise = inputField.getText.toUpperCase.replaceAll("V", "v")

        if ("" != premise.trim && premise != null) {
          dataModel.addPremise(premise)
          getContentPane.repaint()
          getContentPane.validate()
        } else {
          JOptionPane.showMessageDialog(null, "You need to type something!", "Something wrong", JOptionPane.INFORMATION_MESSAGE)
        }

      }
    })

    val theoremField = new JTextField(50)
    val theoremLabel = new JLabel("Theorem")
    val verifyTheorem = new JButton("Verify")
    verifyTheorem.setPreferredSize(new Dimension(80, 35))
    theoremLabel.setPreferredSize(premiseLabel.getPreferredSize)

    verifyTheorem.setEnabled(false)

    theoremPanel.add(theoremLabel)
    theoremPanel.add(theoremField)
    theoremPanel.add(verifyTheorem)

    val table = new JTable(dataModel)
    premiseTablePanel.add(new JScrollPane(table), BorderLayout.CENTER)

    table.setAutoResizeMode(JTable.AUTO_RESIZE_NEXT_COLUMN)
    table.getColumnModel.getColumn(0).setPreferredWidth(350)
    table.getColumnModel.getColumn(1).setPreferredWidth(350)

    table.getColumnModel.getColumn(2).setCellRenderer(new ButtonRenderer)
    table.getColumnModel.getColumn(3).setCellRenderer(new ButtonRenderer)

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
