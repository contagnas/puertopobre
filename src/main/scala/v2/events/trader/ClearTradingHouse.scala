package v2.events.trader

import v2.GameState
import v2.events.Event

object ClearTradingHouse extends Event {
  override def validationError(state: GameState): Option[String] = ???
  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
