package ui.premiseView

import java.awt.BorderLayout
import javax.swing.{JPanel, JTabbedPane}

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
  private val theoremProofTablePanel = new TheoremProofTab(ast, tokens)

  start

  private def start = {
    setLayout(new BorderLayout())
    tabbedPane.add("Expression Graph", graphPanel)
    tabbedPane.add("Expression Tokens", tokensPanel)
    tabbedPane.add("Truth Table", truthTablePanel)
    this.add(tabbedPane, BorderLayout.CENTER)
  }
}
