package ui

import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph
import compiler.Solver
import structure.ASTDefinition.{ASTUnary, ASTBinary, NodeProp, AST}
import structure.Token
import structure.TokenCategories._

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{FlowLayout, Font, GridLayout, BorderLayout, Color}
import javax.swing._


/**
  * Created by MarioDiniz on 08/09/16.
  */
class GraphTab(ast: AST, tokens: List[Token]) extends JPanel {
  private val graph = new mxGraph
  private var graphComponent: JComponent = null
  private val statusBar = new JPanel(new FlowLayout(FlowLayout.LEFT))
  private val resultLabel = new JLabel(" ")

  config
  draw

  private def draw: Unit = {
    graphComponent = drawNodes
    add(graphComponent, BorderLayout.CENTER)
    add(toEvaluatePremises, BorderLayout.EAST)

    def drawNodes: JComponent = {
      val graphParent = graph.getDefaultParent
      graph.getModel.beginUpdate

      def loop(node: AST): AnyRef = {
        node match {
          case n: NodeProp => graph.insertVertex(graphParent, null, s"${n.token.value}", 0,0, 150, 30, s"fillColor=${n.nodeColor}")
          case n: ASTBinary => {
            val p = graph.insertVertex(graphParent, null, s"${n.token.category}(${n.token.value})", 0,0, 150, 30, s"fillColor=${n.nodeColor}")
            graph.insertEdge(graphParent, null, "", p, loop(n.child_left))
            graph.insertEdge(graphParent, null, "", p, loop(n.child_right))
            p
          }
          case n: ASTUnary => {
            val p = graph.insertVertex(graphParent, null, s"${n.token.category}(${n.token.value})", 0,0, 150, 30, s"fillColor=${n.nodeColor}")
            graph.insertEdge(graphParent, null, "applied on", p, loop(n.child))
            p
          }
        }
      }

      loop(ast)
      new  mxHierarchicalLayout(graph).execute(graph.getDefaultParent)
      graph.getModel.endUpdate()
      new mxGraphComponent(graph)
    }

    def toEvaluatePremises: JPanel = {
      val premises: Set[Token] = tokens.filter(_.category == Premise).toSet
      val premisesPanel = new JPanel(new GridLayout(0, 1))
      val premisesTuples = premises.map(
        (token) => {
          val panel = new JPanel()
          val label = new JLabel(s"${token.value}")
          val valueTrue = new JRadioButton("True")
          valueTrue.setActionCommand("true")
          valueTrue.setSelected(true)
          val valueFalse = new JRadioButton("False")
          valueFalse.setActionCommand("false")
          val group = new ButtonGroup()
          group.add(valueTrue)
          group.add(valueFalse)
          panel.add(label).getParent.add(valueTrue).getParent.add(valueFalse)
          (token, panel, group)
        }
      ).toList
      premisesTuples.sortBy(_._1.value).foreach(t => premisesPanel.add(t._2))
      val evaluateButton = new JButton("Evaluate")
      evaluateButton.addActionListener(new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = {
          val premisesValues = premisesTuples.map(
            (t) => {
              val value = t._3.getSelection.getActionCommand.toBoolean
              (t._1, value)
            }
          )
          val (result, _) = Solver.solveIt(ast, premisesValues.toMap)
          remove(graphComponent)
          graph.removeCells(graph.getChildVertices(graph.getDefaultParent()))
          graphComponent = drawNodes
          resultLabel.setText(
            s"<html>Result = " +
            s"<font color='${if (result) "green" else "red"}'>${result.toString.toUpperCase}</font>" +
            s"</html>")
          add(graphComponent, BorderLayout.CENTER)
          repaint()
          validate()
        }
      })
      premisesPanel.add(evaluateButton)
      val p = new JPanel()
      p.add(premisesPanel)
      p
    }
  }

  private def config: Unit = {
    setLayout(new BorderLayout())
    resultLabel.setFont(UIManager.getDefaults().getFont("TabbedPane.font"))
    statusBar.add(resultLabel)
    add(statusBar, BorderLayout.SOUTH)
  }

}
