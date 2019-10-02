package v2.events.mayor

import v2.GameState
import v2.components.ColonistLocation.ActiveColonist
import v2.events.{Event, GetPlayerInput}

case class SelectColonistRemoveTarget(target: ActiveColonist) extends Event {
  override def validationError(state: GameState): Option[String] =
    Some(s"You do not have any colonists $target")
      .filter(_ => state.currentPlayerState.colonists.get(target) > 0)

  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event =
    GetPlayerInput[SelectColonistMove]
}
