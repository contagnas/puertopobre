import Building._
import ColonistLocation.ActiveColonist.{OnBuilding, OnIslandTile}
import Good.Corn
import IslandTile.Plantation
import IslandTile.Plantation.CornPlantation

case class RoleState(
  wharfUsed: Boolean = false,
  wharehousesUsed: Int = 0,
  storedSingleGood: Boolean = false,
  storedGoods: Map[Good, Int] = Map.empty,
  selectedIslandTile: Option[IslandTile] = None,
  purchasedBuilding: Option[Building] = None,
  hasShipped: Boolean = false,
)

case class Player(
  money: Int,
  points: Int,
  buildings: Set[Building],
  numberOfGoods: Count[Good],
  colonists: Count[ColonistLocation],
  islandTiles: Count[IslandTile],
  roleState: RoleState
) {
  lazy val goodsProduction: Count[Good] = {
    val plantations = islandTiles.nonZeroItems.collect {
      case plantation: Plantation => plantation
    }

    val producedGoods: Set[(Good, Int)] = plantations.map { plantation =>
      val activePlantations = colonists.get(OnIslandTile(plantation))
      if (plantation == CornPlantation)
        Corn -> activePlantations
      else {
        val producers = buildings.collect {
          case building: ProductionBuilding if building.resource == plantation.good =>
            building
        }

        val productionCapacity = producers.map(producer => colonists.get(OnBuilding(producer))).sum
        plantation.good -> math.min(activePlantations, productionCapacity)
      }
    }
    Count(producedGoods.toArray: _*)
  }

  def canShipPublic(ships: Iterable[Ship]): Boolean = {
    for {
      good <- numberOfGoods.nonZeroItems
      ship <- ships
    } yield ship.acceptsGood(good)
  }.exists(identity)

  def canShipWharf: Boolean =
    numberOfGoods.total > 0 && !roleState.wharfUsed && colonists.exists(OnBuilding(Wharf))

  def warehouseAvailable: Boolean = {
    val largeWarehouse = colonists.get(OnBuilding(LargeWarehouse)) * 2
    val smallWarehouse = colonists.get(OnBuilding(SmallWarehouse))
    largeWarehouse + smallWarehouse - roleState.wharehousesUsed > 0
  }
}

