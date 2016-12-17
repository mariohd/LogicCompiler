package ui.premiseView

import java.awt.BorderLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import structure.ASTDefinition.AST
import structure.Token

/**
  * Created by MarioDiniz on 10/11/16.
  */
class TheoremProofTab(ast: AST, tokens: List[Token]) extends JPanel {
  private val inputPanel = new JPanel()
  private val textArea = new JTextArea(5, 20);

  config
  draw

  private def config : Unit = {
    this.setLayout(new BorderLayout)
  }

  private def draw : Unit = {
    inputPanel.add(new JLabel("Theorem"))
    val theoremField = new JTextField(40)
    val evaluateButton = new JButton("Evaluate")
    inputPanel.add(theoremField)
    inputPanel.add(evaluateButton)
    val me = this
    evaluateButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {


      }
    })

    this.add(inputPanel, BorderLayout.NORTH)
  }

}

