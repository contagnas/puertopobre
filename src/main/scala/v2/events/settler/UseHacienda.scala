package v2.events.settler

import v2.components.Building.Hacienda
import v2.GameState
import v2.components.ColonistLocation.ActiveColonist.OnBuilding
import v2.events.Event

case class UseHacienda(use: Boolean) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState
    if (use && !player.colonists.exists(OnBuilding(Hacienda)))
      Some(s"You do not have an active Hacienda.")
    else if (use && player.islandTiles.total == 9)
      Some(s"You do not have space for any more island tiles")
    else None
  }

  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event = ???
}
