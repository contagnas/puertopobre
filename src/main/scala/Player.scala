import Building._
import ColonistLocation.{OnBuilding, OnIslandTile}
import Good.Corn
import IslandTile.Plantation.CornPlantation
import IslandTile.{Plantation}

case class RoleState(
  wharfAvailable: Boolean = false,
  passedShipping: Boolean = false,
  warehouseUses: Int = 0,
  storingGoods: Boolean = false,
  shippingGoods: Boolean = false,
  colonistPlacingPlayer: Boolean = false
)

case class Player(
  money: Int,
  points: Int,
  buildings: Set[Building],
  numberOfGoods: Map[Good, Int],
  colonists: Map[ColonistLocation, Int],
  islandTiles: Map[IslandTile, Int],
  governor: Boolean,
  activePlayer: Boolean,
  currentPlayer: Boolean,
  roleState: RoleState
) {
  lazy val goodsProduction: Map[Good, Int] = {
    val plantations = islandTiles.keys.collect {
      case plantation: Plantation => plantation
    }

    plantations.map { plantation =>
      val activePlantations = colonists(OnIslandTile(plantation))
      if (plantation == CornPlantation)
        Corn -> activePlantations
      else {
        val producers = buildings.collect {
          case building: ProductionBuilding if building.resource == plantation.good =>
            building
        }

        val productionCapacity = producers.map(producer => colonists(OnBuilding(producer))).sum
        plantation.good -> math.min(activePlantations, productionCapacity)
      }
    }.filter { case (_, amount) => amount > 0 }
      .toMap.withDefaultValue(0)
  }

  def resetRoleState: Player = copy(
    roleState = RoleState(
      wharfAvailable = colonists(OnBuilding(Wharf)) > 0,
      warehouseUses = colonists(OnBuilding(SmallWarehouse)) +
        (2 * colonists(OnBuilding(LargeWarehouse)))
    )
  )
}

