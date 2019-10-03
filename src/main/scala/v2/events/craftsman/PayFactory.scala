package v2.events.craftsman

import v2.GameState
import v2.events.{Event, NextAction}

import monocle.macros.syntax.lens._

object PayFactory extends Event {
  override def run(state: GameState): GameState = {
    val player = state.currentPlayerState
    val uniqueGoodsProduced = player.roleState.goodsProducedThisRound.nonZeroItems.size
    val payout = uniqueGoodsProduced match {
      case 0 => 0
      case 1 => 1
      case 2 => 2
      case 3 => 3
      case 4 => 5
    }

    state.updateCurrentPlayer(_.lens(_.money).modify(_ + payout))
  }

  override def nextEvent(state: GameState): Event = NextAction
}
