package v2.events

import v2.GameState

object NextRound extends Event {
  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event = {
    if (state.gameOver) DetermineGameResult
    else GetPlayerInput[SelectRole]
  }

}
