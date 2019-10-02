package v2.events.craftsman

import v2.GameState
import v2.components.Good
import v2.events.Event

case class SelectExtraGood(good: Option[Good]) extends Event {
  override def validationError(state: GameState): Option[String] = good match {
    case None => None
    case Some(good) =>
      val player = state.currentPlayerState
      if (!player.goodsProduction.contains(good))
        Some(s"You may only select a good you can produce.")
      else if (!state.availableGoods.exists(good))
        Some(s"There is no more $good available")
      else None
  }

  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event = ???
}
