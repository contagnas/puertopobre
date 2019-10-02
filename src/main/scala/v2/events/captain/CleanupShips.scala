package v2.events.captain

import v2.GameState
import v2.events.Event

object CleanupShips extends Event {
  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
