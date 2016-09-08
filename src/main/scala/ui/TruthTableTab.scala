package ui

import java.awt.BorderLayout
import javax.swing.JPanel

import structure.ASTDefinition.AST

/**
  * Created by MarioDiniz on 08/09/16.
  */
class TruthTableTab(ast: AST) extends JPanel {

  private def start = {
    setLayout(new BorderLayout())
  }
}
