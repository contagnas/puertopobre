package v2.events.craftsman

import v2.GameState
import v2.components.Building.Factory
import v2.components.ColonistLocation.ActiveColonist.OnBuilding
import v2.events.{Event, NextAction}

object DistributeGoods extends Event {
  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event =
    if (state.currentPlayerState.colonists.exists(OnBuilding(Factory)))
      PayFactory
    else
      NextAction
}
