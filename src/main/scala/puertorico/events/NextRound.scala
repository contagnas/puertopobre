package puertorico.events

import puertorico.{Count, GameState}
import cats.implicits._
import puertorico.components.Role

object NextRound extends Event {
  override def run(state: GameState): GameState = {
    val addedIncentives = state.availableRoles.foldLeft(Count.empty[Role])(
      (newIncentives, role) => newIncentives.set(role, 1)
    )

    val nextGov = (state.governor + 1) % state.players.length
    state.copy(
      governor = nextGov,
      roleSelector = nextGov,
      currentPlayer = nextGov,
      roleIncentives = state.roleIncentives |+| addedIncentives,
      availableRoles = state.constants.playableRoles
    )
  }

  override def nextEvent(state: GameState): Event = {
    if (state.gameOver) DetermineGameResult
    else GetPlayerInput[SelectRole]
  }

}
