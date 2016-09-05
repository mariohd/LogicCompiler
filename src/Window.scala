import java.awt.event._
import java.awt.{Font, FlowLayout, BorderLayout}
import javax.swing._
import javax.swing.JFrame._

import Structure.ASTDefinition.{ASTUnary, ASTBinary, NodeProp, AST}
import com.mxgraph.layout.hierarchical._
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.{mxGraph}


/**
  * Created by MarioDiniz on 05/09/16.
  */
class Window() extends JFrame("Logic Compiler") {

  private var graph = new mxGraph
  private val graphParent = graph.getDefaultParent
  private var gComponent: mxGraphComponent = null
  private var expression: String = null

  private def generateASTfor(expr: String): Option[AST] = {
    val tokens =  parsing.tokenizer(expr)
    if (parsing.isValidExpression(tokens, expr)) {
      Some(parsing.generateAST(tokens))
    } else {
      None
    }
  }

  config

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
    val t = new JTextField(50)
    panel.add(t)
    val b = new JButton("Execute")
    b.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        expression = t.getText
        if (gComponent != null) getContentPane.remove(gComponent)
        graph.removeCells(graph.getChildVertices(graph.getDefaultParent()))
        drawGraph
      }
    })
    panel.add(b)
    panel
  }

  private def config: Unit = {
    setLayout(new BorderLayout)
    getContentPane.add(inputPanel, BorderLayout.NORTH)
    setDefaultLookAndFeelDecorated(true)
    setDefaultCloseOperation(EXIT_ON_CLOSE)
    pack()
    setLocationRelativeTo(null)
    setVisible(true)
  }


}
