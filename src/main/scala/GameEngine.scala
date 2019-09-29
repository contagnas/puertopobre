import Building.{Factory, LargeWarehouse, SmallWarehouse}
import ColonistLocation.{InSanJuan, OnBuilding}
import Event._
import cats.implicits._

object GameEngine {
  case class TriggersEvent(gameState: GameState, nextEvent: Event) {
    def t = (gameState, nextEvent)
  }

  def step(gameState: GameState, event: Event): (GameState, Event) = {
    event.render()
    val players = gameState.players

    val currentActingPlayer = players.indexWhere(_.currentPlayer)
    val currentActivePlayer = players.indexWhere(_.activePlayer)
    val currentGovernor = players.indexWhere(_.governor)

    def dispatchAction(role: Role): Event = role match {
      case Role.Captain =>
        GetPlayerInput[ShipGoods]
      case Role.Settler => ???
      case Role.Builder => ???
      case Role.Mayor => ???
      case Role.Craftsman => ???
      case Role.Prospector => ???
      case Role.Prospector2 => ???
    }

    event match {
      case gpi@GetPlayerInput() =>
        val input = gpi.parser.prompt.map { prompt =>
          println(prompt)
          scala.io.StdIn.readLine()
        }

        val playerEvent = for {
          event <- gpi.parser.parse(input)
          _ <- EventValidator.validate(event, gameState)
        } yield event

        playerEvent match {
          case Left(error) =>
            println(error)
            TriggersEvent(gameState, gpi).t
          case Right(event) =>
            TriggersEvent(gameState, event).t
        }


      case NextRound =>
        if (gameState.gameOver) {
          TriggersEvent(gameState, GameOver).t
        } else {
          val nextGov = (currentGovernor + 1) % players.length
          val nextTurnPlayers = players.zipWithIndex.map { case (player, seat) =>
            player.copy(
              governor = seat == nextGov,
              activePlayer = seat == nextGov,
              currentPlayer = seat == nextGov
            )
          }

          val newIncentives = gameState.availableRoles.map(_ -> 1).toMap
          val existingIncentives = gameState.roleIncentives

          TriggersEvent(
            gameState.copy(
              players = nextTurnPlayers,
              roleIncentives = newIncentives |+| existingIncentives,
              availableRoles = gameState.constants.allRoles
            ),
            GetPlayerInput[SelectRole]
          ).t
        }


      case NextRole =>
        val nextActivePlayer = (currentActivePlayer + 1) % players.length
        val resetRoleState = players.map(_.resetRoleState)

        val doneWithRole = gameState.copy(
          currentRole = None,
          players = resetRoleState
        )

        if (nextActivePlayer == currentGovernor) {
          TriggersEvent(doneWithRole, NextRound).t
        } else {
          val players = resetRoleState.zipWithIndex.map { case (player, seat) =>
            player.copy(
              activePlayer = seat == nextActivePlayer,
              currentPlayer = seat == nextActivePlayer
            )
          }
          TriggersEvent(
            doneWithRole.copy(
              players = players
            ),
            GetPlayerInput[SelectRole]
          ).t
        }


      case NextAction =>
        ???


      case GameOver =>
        // Let me out! :(
        throw new IllegalStateException("Game is already over")


      case RefillColonists =>
        val totalBuildingSlots = {
          for {
            p <- players
            building <- p.buildings
            slots = building.colonistSlots
          } yield slots
          }.sum

        val filledBuildingSlots = players.flatMap { player =>
          player.colonists.collect {
            case (OnBuilding(_), count) => count
          }
        }.sum

        val emptyBuildingSlots: Int = totalBuildingSlots - filledBuildingSlots
        val colonistsToAdd = math.min(players.length, emptyBuildingSlots)
        TriggersEvent(
          gameState.copy(
            colonistsOnShip = math.min(colonistsToAdd, gameState.colonistsInSupply),
            colonistsInSupply = math.max(0, gameState.colonistsInSupply - colonistsToAdd)
          ),
          NextRole
        ).t


      case CleanUpShip(ship) =>
        TriggersEvent(
          gameState.copy(
            ships = gameState.ships.updatedWith(ship)(_.map(_.clearShip))
          ),
          NextRole
        ).t


      case ClearTradingHouse =>
        TriggersEvent(
          gameState.copy(
            tradingHouse = Nil
          ),
          NextRole
        ).t


      case RevealNewPlantations =>
        val (hiddenPlantations, shownPlantations) = IslandTile.revealPlantations(
          gameState.hiddenPlantations,
          gameState.constants.numberOfShownPlantations,
          gameState.constants.rng
        )
        TriggersEvent(
          gameState.copy(
            hiddenPlantations = hiddenPlantations,
            shownPlantations = shownPlantations
          ),
          NextRole
        ).t


      case DistributeColonists =>
        val takeFromSupply = gameState.copy(
          colonistsInSupply = gameState.colonistsInSupply - 1,
          players = players.map( p =>
            if (p.activePlayer) {
              p.copy(colonists = p.colonists.updated(InSanJuan, p.colonists(InSanJuan) + 1))
            } else p
          )
        )

        val colonistsToDistribute = gameState.colonistsOnShip
        val colonistsToEach = players.length / colonistsToDistribute
        val fairColonists = colonistsToEach * players.length
        val leftover = colonistsToDistribute - fairColonists
        val activeSeat = players.indexWhere(_.activePlayer)
        val takesExtra = (0 until leftover).map( i =>
          activeSeat + i
        ).toSet

        val distributedColonists = takeFromSupply.copy(
          colonistsOnShip = 0,
          players = takeFromSupply.players.zipWithIndex.map { case (player, seat) =>
            val extra = if (takesExtra.contains(seat)) 1 else 0
            player.copy(
              colonists = player.colonists.updated(InSanJuan, player.colonists(InSanJuan) + colonistsToEach + extra)
            )
          }
        )

        TriggersEvent(distributedColonists, PopulateColonistShip).t


      case DistributeGoods =>
        val player = players(currentActingPlayer)

        val producedGoods = {
          for {
            (good, producable) <- player.goodsProduction.toList
            available = gameState.availableGoods(good)
          } yield good -> math.max(available, producable)
          }.toMap

        val allocatedGoods = players.map { p =>
          if (p.currentPlayer) {
            p.copy(
              numberOfGoods = p.numberOfGoods |+| producedGoods
            )
          } else p
        }

        val nextState = gameState.copy(players = allocatedGoods)
        if (player.colonists(OnBuilding(Factory)) > 0)
          TriggersEvent(nextState, PayFactory(producedGoods.size)).t
        else
          TriggersEvent(nextState, NextAction).t


      case PopulateColonistShip =>
        val vacantBuildingSlots = {
          for {
            player <- players
            building <- player.buildings
            occupiedSlots = player.colonists(OnBuilding(building))
          } yield building.colonistSlots - occupiedSlots
          }.sum

        val numberToPopulate = math.min(vacantBuildingSlots, gameState.constants.minimumColonistsOnShip)

        TriggersEvent(
          gameState.copy(
            colonistsOnShip = numberToPopulate,
            colonistsInSupply = gameState.colonistsInSupply - numberToPopulate
          ),
          NextRole
        ).t


      case PayFactory(numberOfGoods: Int) =>
        val payout = numberOfGoods match {
          case 0 => 0
          case 1 => 1
          case 2 => 2
          case 3 => 3
          case 4 => 5
        }

        TriggersEvent(
          gameState.copy(
            players = players.map(p =>
              p.copy(money = p.money + payout)
            )
          ),
          NextAction
        ).t


      case SelectRole(role) =>
        TriggersEvent(
          gameState.copy(
            currentRole = Some(role),
            availableRoles = gameState.availableRoles - role
          ),
          dispatchAction(role)
        ).t


      case ShipGoods(ship, good) =>
        val game = gameState.copy(
          ships = ships.updated
        )



      case StoreGoods(good, usingWarehouse) =>
        ???


      case SellGood(good) =>
        ???


      case UseHacienda(use) =>
        ???


      case TakeIslandTisle(plantation) =>
        ???


      case UseHospice(use) =>
        ???


      case PurchaseBuilding(building) =>
        ???


      case UseUniversity(use) =>
        ???


      case RearrangeColonists(newArrangement) =>
        ???


      case SelectExtraGood(good) =>
        ???
    }
  }

}
