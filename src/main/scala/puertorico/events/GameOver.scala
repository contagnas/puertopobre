package puertorico.events

import puertorico.GameState

case class GameOver() extends Event { self =>
  override def run(state: GameState): GameState = state
  override def nextEvent(state: GameState): Event = self
}
