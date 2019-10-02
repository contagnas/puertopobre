package v2.events

import v2.GameState

case class GameOver() extends Event {
  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
