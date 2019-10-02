package v2.events.settler

import v2.components.ColonistLocation.ActiveColonist.OnBuilding
import v2.GameState
import v2.components.Building.Hospice
import v2.events.Event

case class UseHospice(use: Boolean) extends Event {
  override def validationError(state: GameState): Option[String] =
    Some("You do not have an active Hospice")
      .filter(_ => use)
      .filter(_ => state.currentPlayerState.colonists.exists(OnBuilding(Hospice)))

  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
