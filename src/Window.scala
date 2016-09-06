import java.awt.event._
import java.awt.{Color, BorderLayout, Font, FlowLayout, GridLayout}
import javax.swing._
import javax.swing.JFrame._

import Structure.ASTDefinition.{ASTUnary, ASTBinary, NodeProp, AST}
import Structure.TokenCategories._
import Structure.Token

import com.mxgraph.layout.hierarchical._
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.{mxGraph}

import scala.collection.mutable.ListBuffer


/**
  * Created by MarioDiniz on 05/09/16.
  */
class Window() extends JFrame("Logic Compiler") {

  private val graph = new mxGraph
  private val graphParent = graph.getDefaultParent
  private var gComponent: mxGraphComponent = null
  private var expression: String = null
  private var premisesTuples: ListBuffer[(Token, JPanel, ButtonGroup)] = ListBuffer()
  private var premisePanel: JPanel = null
  private val textField: JTextField = new JTextField(50)
  private val evaluateButton: JButton = new JButton("Evaluate")
  private val jLabelPanel = new JPanel(new FlowLayout())
  private var ast: AST = null

  config

  private def identifyPremises(tokens: List[Token], ast: AST) : Unit = {
    premisesTuples.clear()
    val premises: Set[Token] = tokens.filter(_.category == Premise).toSet
    premises.foreach( p => premisesTuples += generatePremiseFields(p) )
    val panel = new JPanel(new GridLayout(premises.size + 1,1))
    premisesTuples.sortBy((t) => t._1.value).foreach(t => panel.add(t._2))
    panel.add(evaluateButton)
    premisePanel = panel
    getContentPane.add(premisePanel, BorderLayout.EAST)
  }

  private def generatePremiseFields(premise: Token): (Token, JPanel, ButtonGroup) = {
    val panel = new JPanel(new FlowLayout())
    val label = new JLabel(premise.value)
    val valueTrue = new JRadioButton("True")
    valueTrue.setActionCommand("true")
    valueTrue.setSelected(true)
    val valueFalse = new JRadioButton("False")
    valueFalse.setActionCommand("false")
    val group = new ButtonGroup()
    group.add(valueTrue)
    group.add(valueFalse)
    panel.add(label)
    panel.add(valueTrue)
    panel.add(valueFalse)
    (premise, panel, group)
  }

  private def generateASTfor(expr: String): Option[AST] = {
    val tokens =  parsing.tokenizer(expr)
    if (parsing.isValidExpression(tokens, expr)) {
      ast = parsing.generateAST(tokens)
      identifyPremises(tokens, ast)
      Some(ast)
    } else {
      None
    }
  }

  private def setNodes: Unit = {
    graph.getModel.beginUpdate()

    def loop(node: AST): AnyRef = {
      node match {
        case n: NodeProp => graph.insertVertex(graphParent, null, s"${n.token.value}", 0,0, 150, 30)
        case n: ASTBinary => {
          val p = graph.insertVertex(graphParent, null, s"${n.token.category}(${n.token.value})", 0,0, 150, 30)
          graph.insertEdge(graphParent, null, "", p, loop(n.child_left))
          graph.insertEdge(graphParent, null, "", p, loop(n.child_right))
          p
        }
        case n: ASTUnary => {
          val p = graph.insertVertex(graphParent, null, s"${n.token.category}(${n.token.value})", 0,0, 150, 30)
          graph.insertEdge(graphParent, null, "applied on", p, loop(n.child))
          p
        }
      }
    }

    try { loop(generateASTfor(expression).get) } catch {
      case e: Exception =>
        val font = new Font( "Monospaced", Font.PLAIN, 12 );
        UIManager.put("OptionPane.messageFont", font);

        JOptionPane.showMessageDialog(getContentPane, e.getMessage, "Invalid", JOptionPane.WARNING_MESSAGE)
    }

    new  mxHierarchicalLayout(graph).execute(graph.getDefaultParent)
    graph.getModel.endUpdate()
  }

  private def drawGraph: Unit = {
    setNodes
    gComponent = new mxGraphComponent(graph)
    getContentPane.add(gComponent, BorderLayout.CENTER)
    pack()
  }

  private def inputPanel: JPanel = {
    val panel = new JPanel(new FlowLayout)
    panel.add(textField)
    val b = new JButton("Execute")
    b.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        _resetGraph
      }
    })
    panel.add(b)
    panel
  }

  private def _resetGraph: Unit = {
    if (gComponent != null && premisePanel != null) {
      getContentPane.remove(gComponent)
      getContentPane.remove(premisePanel)
      getContentPane.remove(jLabelPanel)
      graph.removeCells(graph.getChildVertices(graph.getDefaultParent()))
    }
    expression = textField.getText
    drawGraph
  }

  private def config: Unit = {
    setLayout(new BorderLayout)
    getContentPane.add(inputPanel, BorderLayout.NORTH)
    setDefaultLookAndFeelDecorated(true)
    evaluateButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val premisesValues = premisesTuples.map(
          (t) => {
            val value = t._3.getSelection.getActionCommand.toBoolean
            (t._1, value)
          }
        )
        val font = new Font( "Monospaced", Font.BOLD, 30);
        val result = Solver.solveIt(ast, premisesValues.toMap)


        val jlabel = new JLabel(result.toString.toUpperCase())
        jlabel.setForeground(if (result) new Color(60,179,113) else Color.RED)
        jlabel.setFont(font)
        jLabelPanel.removeAll()
        jLabelPanel.repaint()
        jLabelPanel.validate()
        jLabelPanel.add(jlabel)
        getContentPane.add(jLabelPanel, BorderLayout.SOUTH)
        pack
      }
    })
    setDefaultCloseOperation(EXIT_ON_CLOSE)
    pack()
    setLocationRelativeTo(null)
    setVisible(true)
  }
}
