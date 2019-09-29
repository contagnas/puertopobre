import enumeratum._

sealed trait Role extends EnumEntry

object Role extends Enum[Role] {
  case object Captain extends Role
  case object Trader extends Role
  case object Settler extends Role
  case object Builder extends Role
  case object Mayor extends Role
  case object Craftsman extends Role
  case object Prospector extends Role
  case object Prospector2 extends Role

  override def values: IndexedSeq[Role] = findValues
}
