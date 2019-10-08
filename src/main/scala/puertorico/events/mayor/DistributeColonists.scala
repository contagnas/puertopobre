package puertorico.events.mayor

import puertorico.GameState
import puertorico.components.ColonistLocation.InSanJuan
import puertorico.events.{Event, GetPlayerInput}

object DistributeColonists extends Event {
  override def run(state: GameState): GameState =
    if (state.colonistsOnShip == 0)
      // Hack: We already distributed colonists, now we're going taking turns allocating them
      state
    else {
      val players = state.players

      val colonistsToDistribute = state.colonistsOnShip
      val colonistsToEach = colonistsToDistribute / players.length
      val fairColonists = colonistsToEach * players.length
      val leftover = colonistsToDistribute - fairColonists
      // Seats which receive an extra colonist from the boat
      val takesExtra = (0 until leftover).map(i =>
        (state.roleSelector + i) % state.players.length
      ).toSet

      state.copy(
        colonistsInSupply = state.colonistsInSupply - 1,
        colonistsOnShip = 0,
        players = state.players.zipWithIndex.map { case (player, seat) =>
          val mayorBonus = if (seat == state.roleSelector) 1 else 0
          val extra = if (takesExtra.contains(seat)) 1 else 0
          val colonistsGained  = colonistsToEach + extra + mayorBonus

          player.copy(
            colonists = player.colonists.update(InSanJuan, _ + colonistsGained)
          )
        }
      )
    }

  override def nextEvent(state: GameState): Event =
    GetPlayerInput[SelectColonistMove]
}
