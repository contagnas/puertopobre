package v2.events

import v2.GameState

object NextRole extends Event {
  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event =
    if (state.roleSelector == state.governor)
      NextRound
    else
      GetPlayerInput[SelectRole]
}
