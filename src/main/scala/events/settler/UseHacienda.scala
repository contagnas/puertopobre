package v2.events.settler

import monocle.macros.syntax.lens._
import v2.GameState
import v2.components.Building.Hacienda
import v2.components.ColonistLocation.ActiveColonist.OnBuilding
import v2.events.{Enumerable, Event, GetPlayerInput}

case class UseHacienda(use: Boolean) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState
    if (use && !player.colonists.exists(OnBuilding(Hacienda)))
      Some(s"You do not have an active Hacienda.")
    else if (use && player.islandTiles.total == 9)
      Some(s"You do not have space for any more island tiles")
    else if (use && state.hiddenPlantations.total == 0)
      Some("There are no more island tiles to take.")
    else None
  }

  override def run(state: GameState): GameState =
    if (!use) state else {
      val (remaining, drawnPlantation) =
        state.hiddenPlantations.takeRandom(state.constants.rng)

      state.updateCurrentPlayer(
        _.lens(_.islandTiles).modify(_.update(drawnPlantation, _ + 1))
      ).copy(hiddenPlantations = remaining)
    }

  override def nextEvent(state: GameState): Event =
    GetPlayerInput[TakeIslandTile]
}

object UseHacienda {
  implicit val enum = new Enumerable[UseHacienda] {
    override def allPossibleMoves: Seq[UseHacienda] =
      List(true, false).map(UseHacienda.apply)
  }
}
