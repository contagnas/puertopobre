package v2.components

import enumeratum._
import v2.Count
import v2.components.Good._

import scala.util.Random

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

  def revealPlantations(
    hiddenPlantations: Count[Plantation],
    count: Int,
    rng: Random
  ): (Count[Plantation], Count[Plantation]) = (1 to count)
    .foldLeft((hiddenPlantations, Count.empty[Plantation])) {
      case ((hidden, shown), _) =>
        val (stillHidden, justShown) = hidden.takeRandom(rng)
        (stillHidden, shown.update(justShown, _ + 1))
    }

  override def values: IndexedSeq[IslandTile] = findValues ++ Plantation.values
}
