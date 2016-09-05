import javax.swing.{ JFrame}

import Structure.ASTDefinition._
import com.mxgraph.layout._
import com.mxgraph.layout.hierarchical._
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph

/**
  * Created by MarioDiniz on 31/08/16.
  */
object main {
  def main(args: Array[String]): Unit = {
    val expression = "~(XvY) -> (~P^~Q)" // deMorgan
    //val expression = "~P -> X" // deMorgan
    val tokens = parsing.tokenizer(expression)
    if (parsing.isValidExpression(tokens, expression))
      new MyWindow2(parsing.generateAST(tokens))
  }

  class MyWindow2(nodes: AST) extends JFrame("Logic Compiler") {
    import JFrame._

    var graph = new mxGraph();
    var parentg = graph.getDefaultParent();

    graph.getModel().beginUpdate();
    try
    {
      def loop(node: AST, m: Int = 1): AnyRef = {
        node match {
          case n: NodeProp => graph.insertVertex(parentg, null, s"${n.token.value}", 0,0, 150, 30)
          case n: ASTBinary => {
            val p = graph.insertVertex(parentg, null, s"${n.token.category}(${n.token.value})", 0,0, 150, 30)
            val vertexLeft = loop(n.child_left, m + 1)
            val vertexRight = loop(n.child_right , m + 1)
            graph.insertEdge(parentg, null, "", p, vertexLeft)
            graph.insertEdge(parentg, null, "", p, vertexRight)
            p
          }
          case n: ASTUnary => {
            val p = graph.insertVertex(parentg, null, s"${n.token.category}(${n.token.value})", 0,0, 150, 30)
            graph.insertEdge(parentg, null, "", p, loop(n.child, m + 1))
            p
          }
        }
      }

      loop(nodes)

    }
    finally
    {

      var layout = new  mxCompactTreeLayout(graph);
      var cell = graph.getDefaultParent();
      layout.execute(cell);
      graph.getModel().endUpdate();

    }

    var graphComponent = new mxGraphComponent(graph);
    getContentPane().add(graphComponent);


    setDefaultLookAndFeelDecorated(true);

    setDefaultCloseOperation(EXIT_ON_CLOSE);

    pack;
    setLocationRelativeTo(null)
    setVisible(true);
  }


}


