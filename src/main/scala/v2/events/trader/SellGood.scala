package v2.events.trader

import v2.GameState
import v2.components.Building.{LargeMarket, Office, SmallMarket}
import v2.components.ColonistLocation.ActiveColonist.OnBuilding
import v2.components.{Building, Good}
import v2.components.Good._
import v2.events.{Enumerable, Event, NextAction}
import monocle.macros.syntax.lens._
import v2.events.builder.PurchaseBuilding

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

  override def run(state: GameState): GameState = goodToSell match {
    case None => state
    case Some(good) =>
      val player = state.currentPlayerState
      val traderBonus = if (state.currentPlayer == state.roleSelector) 1 else 0
      val sMarketBonus = if (player.colonists.exists(OnBuilding(SmallMarket))) 1 else 0
      val lMarketBonus = if (player.colonists.exists(OnBuilding(LargeMarket))) 2 else 0

      val goodWorth = good match {
        case Corn => 0
        case Indigo => 1
        case Sugar => 2
        case Tobacco => 3
        case Coffee => 4
      }

      val coinsEarned = goodWorth + traderBonus + sMarketBonus + lMarketBonus

      state
        .lens(_.tradingHouse).modify(good :: _)
        .updateCurrentPlayer(
          player => player
            .lens(_.money).modify(_ + coinsEarned)
            .lens(_.numberOfGoods).modify(_.update(good, _ - 1))
        )
  }

  override def nextEvent(state: GameState): Event =
    if (state.tradingHouse.size == 4)
      ClearTradingHouse
    else
      NextAction
}

object SellGood {
  implicit val enum = new Enumerable[SellGood] {
    override def allPossibleMoves: Seq[SellGood] =
      SellGood(None) +: Good.values.map(Some.apply).map(SellGood.apply)
  }
}
