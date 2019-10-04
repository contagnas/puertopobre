package v2.events

import v2.GameState

object DetermineGameResult extends Event {
  override def run(state: GameState): GameState = state
  override def nextEvent(state: GameState): Event = GameOver()
}
