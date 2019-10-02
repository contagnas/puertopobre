package v2.events.builder

import v2.GameState
import v2.components.Building.University
import v2.components.ColonistLocation.ActiveColonist.OnBuilding
import v2.events.Event

case class UseUniversity(use: Boolean) extends Event {
  override def validationError(state: GameState): Option[String] =
    Some(s"You do not have an active University")
      .filter(_ => use)
      .filterNot(_ => state.currentPlayerState.colonists.exists(OnBuilding(University))))

  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
