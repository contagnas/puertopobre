package v2.events.settler

import v2.GameState
import v2.components.Building.ConstructionHut
import v2.components.ColonistLocation.ActiveColonist.OnBuilding
import v2.components.IslandTile
import v2.components.IslandTile.{Plantation, Quarry}
import v2.events.Event

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
        Some(s"There is no $plantation available")
        .filter(_ => state.shownPlantations.contains(plantation))
    }
  }
  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
