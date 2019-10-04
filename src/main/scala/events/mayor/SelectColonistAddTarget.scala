package v2.events.mayor

import monocle.macros.syntax.lens._
import v2.GameState
import v2.components.ColonistLocation.{ActiveColonist, InSanJuan}
import v2.components.ColonistLocation.ActiveColonist.{OnBuilding, OnIslandTile}
import v2.events.{Enumerable, Event, GetPlayerInput}

case class SelectColonistAddTarget(addTarget: ActiveColonist) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState

    addTarget match {
      case OnBuilding(building) =>
        if (!player.buildings.contains(building))
          Some(s"You do not have a $building.")
        else if (building.colonistSlots == player.colonists.get(OnBuilding(building)))
          Some(s"$building is already fully occupied.")
        else
          None
      case OnIslandTile(islandTile) =>
        if (player.islandTiles.get(islandTile) <= player.colonists.get(OnIslandTile(islandTile)))
          Some(s"You do not have an unoccupied $islandTile")
        else None
    }
  }

  override def run(state: GameState): GameState = state.updateCurrentPlayer(
    player => player
      .lens(_.colonists).modify(_.update(addTarget, _ + 1))
      .lens(_.colonists).modify(_.update(InSanJuan, _ - 1))
  )

  override def nextEvent(state: GameState): Event =
    GetPlayerInput[SelectColonistMove]
}

object SelectColonistAddTarget {
  implicit val enum = new Enumerable[SelectColonistAddTarget] {
    override def allPossibleMoves: Seq[SelectColonistAddTarget] =
      ActiveColonist.values.map(SelectColonistAddTarget.apply)
  }
}
