package ui.premiseView

import java.awt.{BorderLayout, Dimension, Font}
import javax.swing._

/**
  * Created by MarioDiniz on 08/09/16.
  */
class PremiseView(premise: String = "") extends JFrame {
  private val premisePanel = new JPanel()
  private var graphPanel: ResultPanel = null

  config
  configInput
  start

  private def start = {
    setSize(new Dimension(640, 480))
    setLocationRelativeTo(null)
    setVisible(true)
  }

  private def config = {
    setLayout(new BorderLayout())
    add(premisePanel, BorderLayout.NORTH)
    setIconImage(new ImageIcon("resources/icon.png").getImage)
    setTitle(s"Premise View")
  }

  private def configInput = {
    val premiseLabel = new JLabel()
    val font = premiseLabel.getFont()
    val boldFont = new Font(font.getFontName(), Font.BOLD, font.getSize())
    premiseLabel.setFont(boldFont)
    premiseLabel.setText(premise)

    premisePanel.add(premiseLabel)

    graphPanel = new ResultPanel(premise)
    getContentPane.add(graphPanel)
    getContentPane.repaint()
    getContentPane.validate()
  }

}
