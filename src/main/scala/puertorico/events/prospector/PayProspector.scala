package puertorico.events.prospector

import monocle.macros.syntax.lens._
import puertorico.GameState
import puertorico.events.{Event, NextRole}

object PayProspector extends Event {
  override def run(state: GameState): GameState =
    state.updateCurrentPlayer(_.lens(_.money).modify(_ + 1))

  override def nextEvent(state: GameState): Event = NextRole
}
