package ui.proofView

import compiler.Converter

/**
  * Created by MarioDiniz on 16/12/16.
  */
class PremiseDataObject(p: String) {
  val premise = p
  val cnfForm: String = Converter.toCNF(this.premise)
}
