package v2.events.mayor

import v2.GameState
import v2.events.{Event, NextRole}

object PopulateColonistShip extends Event {
  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = NextRole
}
