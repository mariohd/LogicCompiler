package ui.proofView

import java.awt.{Color, Component}
import javax.swing.{UIManager, JTable, JButton}
import javax.swing.table.TableCellRenderer
/**
  * Created by MarioDiniz on 16/12/16.
  */
class ButtonRenderer extends JButton with TableCellRenderer {
  override def getTableCellRendererComponent(table: JTable, value: Object, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {
    if (isSelected) {
      setForeground(Color.blue)
      setBackground(table.getSelectionBackground())
    } else {
      setForeground(table.getForeground())
      setBackground(UIManager.getColor("Button.background"))
    }
    if (value != null) {
      setText(value.toString)
    } else {
      setText("")
    }
    return this
  }
}
