package v2.components

import enumeratum._

sealed trait Good extends EnumEntry

object Good extends Enum[Good] {
  case object Corn extends Good
  case object Indigo extends Good
  case object Sugar extends Good
  case object Tobacco extends Good
  case object Coffee extends Good

  override def values: IndexedSeq[Good] = findValues
}
