package ui

import java.awt.BorderLayout
import javax.swing.{JTabbedPane, JPanel}

import compiler.Parser

/**
  * Created by MarioDiniz on 08/09/16.
  */
class ResultPanel(expression: String) extends JPanel {
  private val tokens = Parser.tokenizer(expression)
  private val ast = Parser.generateAST(tokens)

  private val tabbedPane = new JTabbedPane

  private val graphPanel = new GraphTab(ast, tokens)
  private val tokensPanel = new TokensTab(tokens)
  private val truthTablePanel = new TruthTableTab(ast, tokens)

  start

  private def start = {
    setLayout(new BorderLayout())
    tabbedPane.add("Expression Graph", graphPanel)
    tabbedPane.add("Expression Tokens", tokensPanel)
    tabbedPane.add("Truth Table", truthTablePanel)
    this.add(tabbedPane, BorderLayout.CENTER)
  }
}
