package ui

import java.awt.BorderLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JButton, JTextField, JLabel, JPanel}

import compiler.Proof
import structure.ASTDefinition.AST
import structure.Token

/**
  * Created by MarioDiniz on 10/11/16.
  */
class TheoremProofTab(ast: AST, tokens: List[Token]) extends JPanel {
  private val inputPanel = new JPanel()

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

        me.add(new GraphTab(Proof.removeImplications(ast, tokens), tokens), BorderLayout.SOUTH);
      }
    })

    this.add(inputPanel, BorderLayout.NORTH)
  }

}

