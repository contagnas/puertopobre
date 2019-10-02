package v2.events.craftsman

import v2.GameState
import v2.events.Event

case class PayFactory() extends Event {
  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
