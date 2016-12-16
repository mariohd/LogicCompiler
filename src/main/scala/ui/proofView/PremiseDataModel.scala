package ui.proofView

import javax.swing.table.AbstractTableModel

import scala.collection.mutable.ListBuffer

/**
  * Created by MarioDiniz on 16/12/16.
  */
class PremiseDataModel extends AbstractTableModel {

  var premises = ListBuffer[PremiseDataObject]()

  def addPremise(premise: String): Unit = {
    val premiseDO = new PremiseDataObject(premise)
    premises += premiseDO

    this.fireTableDataChanged()
  }

  def removePremise(premiseId: Integer): Unit = {
    premises.remove(premiseId)
    this.fireTableDataChanged()
  }

  override def getRowCount: Int = premises.size

  override def getColumnCount: Int = 4

  override def getColumnName(i: Int): String = {
    i match {
      case 0 => "Premise"
      case 1 => "CNF"
      case 2 => "Viewer"
      case 3 => "Remove"
    }
  }

  override def getValueAt(row: Int, col: Int): Object = {
    col match {
      case 0 => premises.lift(row).get.premise
      case 1 => premises.lift(row).get.cnfForm
      case 2 => "View"
      case 3 => "Delete"
    }
  }

}
