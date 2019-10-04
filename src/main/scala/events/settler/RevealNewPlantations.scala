package v2.events.settler

import cats.implicits._
import v2.components.IslandTile
import v2.events.{Event, NextRole}
import v2.{Count, GameState}

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
