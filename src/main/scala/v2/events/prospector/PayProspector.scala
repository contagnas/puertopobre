package v2.events.prospector

import v2.GameState
import v2.events.{Event, NextRole}

object PayProspector extends Event {
  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = NextRole
}
