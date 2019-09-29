import Good._

import enumeratum._

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
    hiddenPantations: Map[Plantation, Int],
    count: Int,
    rng: Random
  ): (Map[Plantation, Int], List[Plantation]) = (1 to count)
    .foldLeft((hiddenPantations, List.empty[Plantation])) { case ((hidden, shown), _) =>
      val (stillHidden, justShown) = IslandTile.revealPlantation(hidden, rng)
      (stillHidden, justShown :: shown)
    }

  private def revealPlantation(
    hiddenPlantations: Map[Plantation, Int],
    rng: Random
  ): (Map[Plantation, Int], Plantation) = {
    val totalWeight = hiddenPlantations.values.sum
    val selectedWeight = rng.between(0, totalWeight)

    val plantationCDF: Seq[(Plantation, Int)] = hiddenPlantations.toList
      .scanLeft[(Plantation, Int)]((null, 0)) { case ((_, weight), (plantation, count)) =>
        (plantation, weight + count)
      }

    val selectedPlantation = plantationCDF.find { case (_, p) => p > selectedWeight }
      .get._1

    val stillHidden = hiddenPlantations.updated(selectedPlantation, hiddenPlantations(selectedPlantation) - 1)
    (stillHidden, selectedPlantation)
  }

  override def values: IndexedSeq[IslandTile] = findValues ++ Plantation.values
}
