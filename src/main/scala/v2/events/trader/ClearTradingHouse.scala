package v2.events.trader

import v2.GameState
import v2.events.{Event, NextRole}

object ClearTradingHouse extends Event {
  override def validationError(state: GameState): Option[String] = ???

  override def run(state: GameState): GameState =
    state.copy(tradingHouse = Nil)

  // Note clearing the trading house immediately ends trading.
  override def nextEvent(state: GameState): Event = NextRole
}
