import enumeratum._

sealed trait ColonistLocation extends EnumEntry
object ColonistLocation extends Enum[ColonistLocation] {
  case object InSanJuan extends ColonistLocation

  sealed trait ActiveColonist extends ColonistLocation with EnumEntry
  object ActiveColonist extends Enum[ActiveColonist] {
    case class OnBuilding(building: Building) extends ActiveColonist
    case class OnIslandTile(islandTile: IslandTile) extends ActiveColonist

    override def values: IndexedSeq[ActiveColonist] = findValues
  }

  override def values: IndexedSeq[ColonistLocation] = findValues ++ ActiveColonist.values
}

sealed trait ColonistMovement extends EnumEntry
object ColonistMovement extends Enum[ColonistMovement] {
  case object Add extends ColonistMovement
  case object Remove extends ColonistMovement
  case object Finish extends ColonistMovement

  override def values: IndexedSeq[ColonistMovement] = findValues
}
