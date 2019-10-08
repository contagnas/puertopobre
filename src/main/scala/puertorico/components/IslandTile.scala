package puertorico.components

import enumeratum._
import puertorico.{Count, GameState}
import Good._

sealed trait IslandTile extends EnumEntry

object IslandTile extends Enum[IslandTile] {
  sealed abstract class Plantation(val good: Good) extends IslandTile with EnumEntry
  object Plantation extends Enum[Plantation] {
    case object CornPlantation extends Plantation(Corn)
    case object IndigoPlantation extends Plantation(Indigo)
    case object SugarPlantation extends Plantation(Sugar)
    case object TobaccoPlantation extends Plantation(Tobacco)
    case object CoffeePlantation extends Plantation(Coffee)

    override def values: IndexedSeq[Plantation] = findValues
  }

  case object Quarry extends IslandTile

  def revealPlantations(state: GameState): GameState = {
    var updatedState = state
    for (_ <- 1 to state.constants.numberOfShownPlantations) {
      if (updatedState.hiddenPlantations.total == 0) {
        updatedState = updatedState.copy(
          discardedPlantations = Count.empty,
          hiddenPlantations = updatedState.discardedPlantations
        )
      }

      if (updatedState.hiddenPlantations.total > 0) {
        val (stillHidden, revealed) = updatedState.hiddenPlantations.takeRandom(state.constants.rng)
        updatedState = updatedState.copy(
          hiddenPlantations = stillHidden,
          shownPlantations = updatedState.shownPlantations.update(revealed, _ + 1)
        )
      }
    }

    updatedState
  }

  override def values: IndexedSeq[IslandTile] = findValues ++ Plantation.values
}
