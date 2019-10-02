package v2.events.trader

import v2.GameState
import v2.components.Building.Office
import v2.components.ColonistLocation.ActiveColonist.OnBuilding
import v2.components.Good
import v2.events.{Event, NextAction}

case class SellGood(goodToSell: Option[Good]) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState

    goodToSell match {
      case None => None
      case Some(good) =>
        if (!player.numberOfGoods.exists(good))
          Some(s"You do not have any $good.")
        else if (state.tradingHouse.contains(good) && !player.colonists.exists(OnBuilding(Office)))
          Some(s"A $good has already been sold in the trading house.")
        else
          None
    }
  }

  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event =
    if (state.tradingHouse.size == 4)
      ClearTradingHouse
    else
      NextAction
}

