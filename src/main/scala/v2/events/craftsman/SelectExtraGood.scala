package v2.events.craftsman

import v2.GameState
import v2.components.Good
import v2.events.{Enumerable, Event, NextRole}
import monocle.macros.syntax.lens._

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

  override def run(state: GameState): GameState = good match {
    case None => state
    case Some(good) =>
      state.updateCurrentPlayer(
        _.lens(_.numberOfGoods).modify(_.update(good, _ + 1))
      ).lens(_.availableGoods).modify(_.update(good, _ - 1))
  }

  override def nextEvent(state: GameState): Event = NextRole
}

object SelectExtraGood {
  implicit val enum = new Enumerable[SelectExtraGood] {
    override def allPossibleMoves: Seq[SelectExtraGood] =
      SelectExtraGood(None) +: Good.values.map(Some.apply).map(SelectExtraGood.apply)
  }
}
