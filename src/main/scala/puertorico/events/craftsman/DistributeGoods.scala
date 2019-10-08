package puertorico.events.craftsman

import monocle.macros.syntax.lens._
import puertorico.GameState
import puertorico.components.Building.Factory
import puertorico.components.ColonistLocation.ActiveColonist.OnBuilding
import puertorico.events.{Event, NextAction}

object DistributeGoods extends Event {
  override def run(state: GameState): GameState = {
    val player = state.currentPlayerState
    val producedGoods = {
      for {
        (good, producable) <- player.goodsProduction.toList
        available = state.availableGoods.get(good)
      } yield good -> math.min(available, producable)
      }.toArray

    producedGoods.foldLeft(state) { case (state, (good, number)) =>
      state.updateCurrentPlayer(
        _.lens(_.numberOfGoods).modify(_.update(good, _ + number))
      ).lens(_.availableGoods).modify(_.update(good, _ - number))
    }
  }

  override def nextEvent(state: GameState): Event =
    if (state.currentPlayerState.colonists.exists(OnBuilding(Factory)))
      PayFactory
    else
      NextAction
}
