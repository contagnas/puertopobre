sealed trait ColonistLocation
object ColonistLocation {
  case object InSanJuan extends ColonistLocation
  case class OnBuilding(building: Building) extends ColonistLocation
  case class OnIslandTile(islandTile: IslandTile) extends ColonistLocation
}
