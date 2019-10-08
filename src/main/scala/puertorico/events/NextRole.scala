package puertorico.events

import puertorico.{GameState, RoleState}

object NextRole extends Event {
  override def run(state: GameState): GameState = {
    val nextRoleSelector = (state.roleSelector + 1) % state.players.length

    state.copy(
      roleSelector = nextRoleSelector,
      currentRole = None,
      players = state.players.map(_.copy(roleState = RoleState())),
      currentPlayer = nextRoleSelector
    )
  }

  override def nextEvent(state: GameState): Event =
    if (state.roleSelector == state.governor)
      NextRound
    else
      GetPlayerInput[SelectRole]
}
