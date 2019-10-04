package v2.events.settler

import v2.components.ColonistLocation.ActiveColonist.{OnBuilding, OnIslandTile}
import v2.GameState
import v2.components.Building.Hospice
import v2.events.{Enumerable, Event, NextAction}
import monocle.macros.syntax.lens._
import v2.events.captain.DecideToShip

case class UseHospice(use: Boolean) extends Event {
  override def validationError(state: GameState): Option[String] =
    Some("You do not have an active Hospice")
      .filter(_ => use)
      .filter(_ => state.currentPlayerState.colonists.exists(OnBuilding(Hospice)))

  override def run(state: GameState): GameState =
    if (!use) state else {
      state.currentPlayerState.roleState.selectedIslandTile match {
        case None => state
        case Some(tile) =>
          state.updateCurrentPlayer(
            _.lens(_.colonists).modify(_.update(OnIslandTile(tile), _+ 1))
          ).lens(_.colonistsInSupply).modify(_ - 1)
      }
    }

  override def nextEvent(state: GameState): Event = NextAction
}

object UseHospice {
  implicit val enum = new Enumerable[UseHospice] {
    override def allPossibleMoves: Seq[UseHospice] =
      List(true, false).map(UseHospice.apply)
  }
}
