package ui

import java.awt.{Dimension, BorderLayout}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

/**
  * Created by MarioDiniz on 08/09/16.
  */
class UserInterface extends JFrame {
  private val inputPanel = new JPanel()
  private var graphPanel: ResultPanel = null

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
    setIconImage(new ImageIcon("resources/icon.png").getImage)
    setTitle("Logic Compiler")
  }

  private def configInput = {
    val inputField = new JTextField(50)
    val executeButton = new JButton("Execute")
    inputPanel.add(inputField)
    inputPanel.add(executeButton)

    executeButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        if (graphPanel != null)
          getContentPane.remove(graphPanel)
        graphPanel = new ResultPanel(inputField.getText)
        getContentPane.add(graphPanel)
        getContentPane.repaint()
        getContentPane.validate()
      }
    })
    getContentPane.repaint()
    getContentPane.validate()
  }

}
