import Building._
import Good._
import IslandTile._
import IslandTile.Plantation._
import Role._
import ShipSize.PublicShipSize._
import ShipSize._

import scala.util.Random

case class GameState(
  players: List[Player],
  currentRole: Option[Role],
  buildingShop: Map[Building, Int],
  hiddenPlantations: Map[Plantation, Int],
  shownPlantations: List[IslandTile],
  quarriesRemaining: Int,
  ships: Map[PublicShipSize, Ship],
  pointsRemaining: Int,
  colonistsInSupply: Int,
  colonistsOnShip: Int,
  roleIncentives: Map[Role, Int],
  availableRoles: Set[Role],
  availableGoods: Map[Good, Int],
  tradingHouse: List[Good],
  constants: GameConstants,
) {
  lazy val gameOver: Boolean = {
    colonistsInSupply == 0 ||
      pointsRemaining == 0 ||
    players.exists(_.buildings.map(b => if (b.largeBuilding) 2 else 1).sum >= 12)
  }
}

case class GameConstants(
  numberOfShownPlantations: Int,
  allRoles: Set[Role],
  minimumColonistsOnShip: Int,
  rng: Random
)

object GameState {
  def initialState(numberOfPlayers: Int, seed: Int = System.currentTimeMillis.toInt): GameState = {
    val allRoles: Set[Role] = List(
      Captain,
      Trader,
      Settler,
      Builder,
      Mayor,
      Craftsman,
      Prospector,
      Prospector2
    ).take(numberOfPlayers + 3).toSet

    val rng = new Random(seed)

    val constants = GameConstants(
      numberOfShownPlantations = numberOfPlayers + 1,
      allRoles = allRoles,
      minimumColonistsOnShip = numberOfPlayers,
      rng = rng
    )

    val emptyShips: Map[PublicShipSize, Ship] = Map(
      Small  -> Ship.makeShip(numberOfPlayers + 1),
      Medium -> Ship.makeShip(numberOfPlayers + 2),
      Large  -> Ship.makeShip(numberOfPlayers + 3)
    )

    val initialPlayerPlantations: Array[Plantation] = numberOfPlayers match {
      case 3 => Array(IndigoPlantation, IndigoPlantation, CornPlantation)
      case 4 => Array(IndigoPlantation, IndigoPlantation, CornPlantation, CornPlantation)
      case 5 => Array(IndigoPlantation, IndigoPlantation, IndigoPlantation, CornPlantation, CornPlantation)
    }

    val initialPlayerBoards = (0 until numberOfPlayers).map { i =>
      val firstPlayer = 0 == i
      val initialPlantation: IslandTile = initialPlayerPlantations(i)
      Player(
        money = numberOfPlayers - 1,
        points = 0,
        buildings = Set.empty,
        numberOfGoods = Map.empty.withDefaultValue(0),
        colonists = Map.empty.withDefaultValue(0),
        islandTiles = Map(initialPlantation -> 1).withDefaultValue(0),
        governor = firstPlayer,
        activePlayer = firstPlayer,
        currentPlayer = firstPlayer,
        roleState = RoleState()
      )
    }.toList

    val totalPoints = numberOfPlayers match {
      case 3 => 75
      case 4 => 100
      case 5 => 122
    }

    val totalColonists = numberOfPlayers match {
      case 3 => 55
      case 4 => 75
      case 5 => 95
    }

    val plantationsAfterInitial = initialPlayerPlantations.foldLeft(allPlantations) { case (total, plantation) =>
      total.updated(plantation, total(plantation) - 1)
    }

    val (hiddenPlantations, shownPlantations) = revealPlantations(
      plantationsAfterInitial,
      constants.numberOfShownPlantations,
      rng
    )

    GameState(
      players = initialPlayerBoards,
      currentRole = None,
      buildingShop = allBuildings,
      hiddenPlantations = hiddenPlantations,
      shownPlantations = shownPlantations,
      quarriesRemaining = 8,
      ships = emptyShips,
      pointsRemaining = totalPoints,
      colonistsInSupply = totalColonists - constants.minimumColonistsOnShip,
      colonistsOnShip = constants.minimumColonistsOnShip,
      roleIncentives = Map.empty.withDefaultValue(0),
      availableRoles = constants.allRoles,
      availableGoods = allResources,
      tradingHouse = List.empty,
      constants = constants
    )
  }

  private val allResources: Map[Good, Int] = Map(
    Coffee -> 9,
    Tobacco -> 9,
    Corn -> 10,
    Sugar -> 11,
    Indigo -> 11
  )

  private val allBuildings: Map[Building, Int] = Map(
    SmallIndigoPlant -> 4,
    SmallSugarMill -> 4,
    SmallMarket -> 2,
    Hacienda -> 2,
    ConstructionHut -> 2,
    SmallWarehouse -> 2,

    IndigoPlant -> 3,
    SugarMill -> 3,
    Hospice -> 2,
    Office -> 2,
    LargeMarket -> 2,
    LargeWarehouse -> 2,

    TobaccoStorage -> 3,
    CoffeeRoaster -> 3,
    Factory -> 2,
    University -> 2,
    Harbor -> 2,
    Building.Wharf -> 2,

    GuildHall -> 1,
    Residence -> 1,
    Fortress -> 1,
    CustomsHouse -> 1,
    CityHall -> 1
  )

  private val allPlantations: Map[Plantation, Int] = Map(
    CornPlantation -> 10,
    IndigoPlantation -> 12,
    SugarPlantation -> 11,
    TobaccoPlantation -> 9,
    CoffeePlantation -> 8
  )

}

