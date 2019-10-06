package v2

import v2.components.Building._
import v2.components.Good._
import v2.components.IslandTile.Plantation._
import v2.components.IslandTile.{Plantation, _}
import v2.components.Role._
import v2.components._
import v2.components.ShipSize.PublicShipSize
import v2.components.ShipSize.PublicShipSize._

import scala.util.Random

case class GameState(
  players: List[Player],
  currentPlayer: Int,
  roleSelector: Int,
  governor: Int,
  currentRole: Option[Role],
  buildingShop: Count[Building],
  hiddenPlantations: Count[Plantation],
  shownPlantations: Count[Plantation],
  discardedPlantations: Count[Plantation] = Count.empty,
  quarriesRemaining: Int,
  ships: Map[PublicShipSize, Ship],
  pointsRemaining: Int,
  colonistsInSupply: Int,
  colonistsOnShip: Int,
  roleIncentives: Count[Role],
  availableRoles: Set[Role],
  availableGoods: Count[Good],
  tradingHouse: List[Good],
  constants: GameConstants,
) {
  lazy val gameOver: Boolean = {
    colonistsInSupply <= 0 ||
      pointsRemaining <= 0 ||
    players.exists(_.buildings.toList.map(b => if (b.largeBuilding) 2 else 1).sum >= 12)
  }

  def currentPlayerState: Player = players(currentPlayer)

  def pretty: String =
    s"""
      |currentRole = $currentRole
      |shownPlantations = $shownPlantations
      |ships = ${ships.values.map(_.toString)}
      |availableRoles = $availableRoles
      |roleIncentives = $roleIncentives
      |""".stripMargin

  def updateCurrentPlayer(f: Player => Player): GameState =
    copy(
      players = players.zipWithIndex.map {
        case (p, seat) => if (seat == currentPlayer) f(p) else p
      }
    )
}

case class GameConstants(
  numberOfShownPlantations: Int,
  playableRoles: Set[Role],
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
      playableRoles = allRoles,
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
      val initialPlantation: IslandTile = initialPlayerPlantations(i)
      Player(
        money = numberOfPlayers - 1,
        points = 0,
        buildings = Set.empty,
        numberOfGoods = Count.empty,
        colonists = Count.empty,
        islandTiles = Count(initialPlantation -> 1),
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

    val plantationsAfterInitial: Count[Plantation] = initialPlayerPlantations.foldLeft(allPlantations) { case (total, plantation) =>
      total.update(plantation, _ - 1)
    }

    val state = GameState(
      players = initialPlayerBoards,
      currentPlayer = 0,
      roleSelector = 0,
      governor = 0,
      currentRole = None,
      buildingShop = allBuildings,
      hiddenPlantations = plantationsAfterInitial,
      shownPlantations = Count.empty,
      quarriesRemaining = 8,
      ships = emptyShips,
      pointsRemaining = totalPoints,
      colonistsInSupply = totalColonists - constants.minimumColonistsOnShip,
      colonistsOnShip = constants.minimumColonistsOnShip,
      roleIncentives = Count.empty,
      availableRoles = constants.playableRoles,
      availableGoods = allResources,
      tradingHouse = List.empty,
      constants = constants
    )

    revealPlantations(state)
  }

  private val allResources: Count[Good] = Count(
    Coffee -> 9,
    Tobacco -> 9,
    Corn -> 10,
    Sugar -> 11,
    Indigo -> 11
  )

  private val allBuildings: Count[Building] = Count(
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

  private val allPlantations: Count[Plantation] = Count(
    CornPlantation -> 10,
    IndigoPlantation -> 12,
    SugarPlantation -> 11,
    TobaccoPlantation -> 9,
    CoffeePlantation -> 8
  )
}
