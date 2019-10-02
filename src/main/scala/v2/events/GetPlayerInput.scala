package v2.events.playerinput

import v2.GameState

case_class GetPlayerInput() extends Event {
  override def validationError(state: GameState): Option[String] = ???
  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
