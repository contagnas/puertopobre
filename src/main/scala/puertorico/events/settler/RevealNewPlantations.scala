package puertorico.events.settler

import cats.implicits._
import puertorico.components.IslandTile
import puertorico.events.{Event, NextRole}
import puertorico.{Count, GameState}

object RevealNewPlantations extends Event {
  override def run(state: GameState): GameState = {
    val untakenDiscarded = state.copy(
      discardedPlantations = state.discardedPlantations |+| state.shownPlantations,
      shownPlantations = Count.empty
    )

    IslandTile.revealPlantations(untakenDiscarded)
  }

  override def nextEvent(state: GameState): Event = NextRole
}
