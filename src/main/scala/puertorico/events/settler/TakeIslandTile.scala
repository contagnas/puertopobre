package puertorico.events.settler

import puertorico.GameState
import puertorico.components.Building.ConstructionHut
import puertorico.components.ColonistLocation.ActiveColonist.OnBuilding
import puertorico.components.IslandTile.{Plantation, Quarry}
import puertorico.events.{Enumerable, Event, GetPlayerInput}
import monocle.macros.syntax.lens._
import puertorico.components.IslandTile

case class TakeIslandTile(islandTile: Option[IslandTile]) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState

    val quarriesAvailable: Either[String, Unit] = Some("There are no quarries left.")
      .filter(_ => state.quarriesRemaining > 0).toLeft(())

    val playerCanTakeQuarry: Either[String, Unit] =
      Some("You cannot take a quarry, take a plantation instead.")
        .filter(_ => state.currentPlayer == state.roleSelector)
        .filter(_ => state.currentPlayerState.colonists.exists(OnBuilding(ConstructionHut)))
        .toLeft(())

    if (islandTile.isDefined && player.islandTiles.total == 9) {
      Some("You do not have space for any more island tiles.")
    } else islandTile match {
      case None => None
      case Some(Quarry) =>
        (for {
          _ <- quarriesAvailable
          canTakeQuarry <- playerCanTakeQuarry
        } yield canTakeQuarry).swap.toOption
      case Some(plantation: Plantation) =>
        if (!state.shownPlantations.contains(plantation))
          Some(s"There is no $plantation available")
        else None
    }
  }
  override def run(state: GameState): GameState = islandTile match {
    case None => state
    case Some(tile) =>
      val updatedPlayer = state.updateCurrentPlayer(
        player => player
          .lens(_.islandTiles).modify(_.update(tile, _ + 1))
          .lens(_.roleState.selectedIslandTile).set(Some(tile))
      )

      tile match {
        case Quarry => updatedPlayer.lens(_.quarriesRemaining).modify(_ - 1)
        case plantation: Plantation => updatedPlayer.lens(_.shownPlantations).modify(_.update(plantation, _ - 1))
      }
  }

  override def nextEvent(state: GameState): Event =
    GetPlayerInput[UseHospice] // Could skip this if player doesn't have one
}

object TakeIslandTile {
  implicit val enum = new Enumerable[TakeIslandTile] {
    override def allPossibleMoves: Seq[TakeIslandTile] =
      TakeIslandTile(None) +: IslandTile.values.map(Some.apply).map(TakeIslandTile.apply)
  }
}
