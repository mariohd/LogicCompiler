package Structure

/**
  * Created by MarioDiniz on 31/08/16.
  */
import Structure.TokenCategories._

case class Token(val t: TokenCategory, val v: String) {
  val category: TokenCategory = t
  val value: String = v

  override def toString(): String = {
    s"[category: ${category}, value: ${value}]"
  }
}
