import ColonistLocation.ActiveColonist.OnIslandTile
import Good._
import IslandTile.Quarry
import enumeratum._

sealed abstract class Building(
  val price: Int,
  val points: Int,
  val colonistSlots: Int,
  val maxQuarries: Int,
  val largeBuilding: Boolean = false
) extends EnumEntry {
  def discountedPrice(gameState: GameState): Int = {
    val builderDiscount = if (gameState.currentPlayer == gameState.roleSelector) 1 else 0
    val player = gameState.players(gameState.currentPlayer)
    val quarries = player.colonists.get(OnIslandTile(Quarry))
    val quarryDiscount = math.min(quarries, maxQuarries)
    price - builderDiscount - quarryDiscount
  }
}

object Building extends Enum[Building] {
  sealed trait ProductionBuilding extends Building {
    val resource: Good
  }

  sealed trait LargeProductionBuilding extends ProductionBuilding
  sealed trait SmallProductionBuilding extends ProductionBuilding

  sealed trait Warehouse extends Building

  case object SmallIndigoPlant extends Building(1, 1, 1, 1)
    with SmallProductionBuilding { override val resource: Good = Indigo }
  case object SmallSugarMill extends Building(2, 1, 1, 1)
    with SmallProductionBuilding { override val resource: Good = Sugar }
  case object SmallMarket extends Building(1, 1, 1, 1)
  case object Hacienda extends Building(2, 1, 1, 1)
  case object ConstructionHut extends Building(2, 1, 1, 1)
  case object SmallWarehouse extends Building(3, 1, 1, 1)
    with Warehouse

  case object IndigoPlant extends Building(3, 2, 3, 2)
    with LargeProductionBuilding { override val resource: Good = Indigo }
  case object SugarMill extends Building(4, 2, 3, 2)
    with LargeProductionBuilding { override val resource: Good = Sugar }
  case object Hospice extends Building(4, 2, 1, 2)
  case object Office extends Building(5, 2, 1, 2)
  case object LargeMarket extends Building(5, 2, 1, 2)
  case object LargeWarehouse extends Building(6, 2, 1, 2)
    with Warehouse

  case object TobaccoStorage extends Building(5, 3, 3, 3)
    with LargeProductionBuilding { override val resource: Good = Tobacco }
  case object CoffeeRoaster extends Building(6, 3, 2, 3)
    with LargeProductionBuilding { override val resource: Good = Coffee }
  case object Factory extends Building(7, 3, 1, 3)
  case object University extends Building(8, 3, 1, 3)
  case object Harbor extends Building(8, 3, 1, 3)
  case object Wharf extends Building(9, 3, 1, 3)

  case object GuildHall extends Building(10, 4, 1, 4, true)
  case object Residence extends Building(10, 4, 1, 4, true)
  case object Fortress extends Building(10, 4, 1, 4, true)
  case object CustomsHouse extends Building(10, 4, 1, 4, true)
  case object CityHall extends Building(10, 4, 1, 4, true)

  sealed trait WarehouseUses
  case object TwoUses extends WarehouseUses
  case object OneUse extends WarehouseUses
  case object NoUses extends WarehouseUses

  override def values: IndexedSeq[Building] = findValues
}
