package v2.events.mayor

import v2.components.ColonistLocation.ActiveColonist.{OnBuilding, OnIslandTile}
import v2.GameState
import v2.components.ColonistLocation.ActiveColonist
import v2.events.{Event, GetPlayerInput}

case class SelectColonistAddTarget(target: ActiveColonist) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState

    target match {
      case OnBuilding(building) =>
        if (!player.buildings.contains(building))
          Some(s"You do not have a $building.")
        else if (building.colonistSlots == player.colonists.get(OnBuilding(building)))
          Some(s"$building is already fully occupied.")
        else
          None
      case OnIslandTile(islandTile) =>
        Some(s"You do not have an unoccupied $islandTile")
          .filter(_ => player.islandTiles.get(islandTile) < player.colonists.get(OnIslandTile(islandTile))
    }
  }

  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event =
    GetPlayerInput[SelectColonistMove]
}
