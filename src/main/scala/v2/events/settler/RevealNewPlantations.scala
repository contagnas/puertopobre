package v2.events.settler

import v2.GameState
import v2.events.Event

object RevealNewPlantations extends Event {
  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}