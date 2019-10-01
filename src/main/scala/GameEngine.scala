import Building.{Factory, Harbor, LargeMarket, SmallMarket}
import ColonistLocation.ActiveColonist._
import ColonistLocation.{ActiveColonist, InSanJuan}
import Event._
import Good._
import IslandTile.{Plantation, Quarry}
import cats.implicits._
import monocle.macros.syntax.lens._

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

    val player = gameState.players(currentActingPlayer)

    def dispatchAction(role: Role): Event = role match {
      case Role.Captain => GetPlayerInput[DecideToShip]
      case Role.Settler => GetPlayerInput[UseHacienda]
      case Role.Builder => GetPlayerInput[PurchaseBuilding]
      case Role.Mayor => DistributeColonists
      case Role.Craftsman => DistributeGoods
      case Role.Prospector => PayProspector
      case Role.Prospector2 => PayProspector
    }

    event match {
      case gpi@GetPlayerInput() =>
        println(gameState.pretty)
        val input = gpi.parser.prompt.map { prompt =>
          println(s"Player ${currentActingPlayer + 1}: $prompt")
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

          val addedIncentives = gameState.availableRoles.foldLeft(gameState.roleIncentives) {
            case (acc, role) => acc.update(role, _ + 1)
          }

          TriggersEvent(
            gameState.copy(
              players = nextTurnPlayers,
              roleIncentives = addedIncentives,
              availableRoles = gameState.constants.allRoles
            ),
            GetPlayerInput[SelectRole]
          ).t
        }


      case NextRole =>
        val nextActivePlayer = (currentActivePlayer + 1) % players.length
        val resetRoleState = players.map(_.copy(roleState = RoleState()))

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

      case PayProspector =>
        val paid = gameState.updateCurrentPlayer(_.lens(_.money).modify (_ + 1))
        TriggersEvent(paid, NextRole).t


      case NextAction =>
        val newCurrentPlayer = (currentActingPlayer + 1) % gameState.players.length
        val finishedActionPhase = newCurrentPlayer == currentActivePlayer
        val nextPlayerState = gameState.copy(
          players = gameState.players.zipWithIndex.map { case (player, seat) =>
            player.copy(currentPlayer = seat == newCurrentPlayer)
          }
        )

        if (gameState.currentRole.isEmpty)
          throw new IllegalStateException("Cannot do next state with no role")

        val role = gameState.currentRole.get
        val next = if (role == Role.Captain) {
          if (gameState.players.exists(
            p => p.canShipPublic(gameState.ships.values) || p.canShipWharf)
          ) {
            TriggersEvent(nextPlayerState, GetPlayerInput[DecideToShip])
          } else {
            TriggersEvent(gameState, CleanupShips)
          }
        } else if (role == Role.Settler && finishedActionPhase) {
          TriggersEvent(gameState, RevealNewPlantations)
        } else if (role == Role.Mayor && finishedActionPhase) {
          TriggersEvent(gameState, PopulateColonistShip)
        } else if (role == Role.Craftsman && finishedActionPhase) {
          val makeActivePlayerCurrent = gameState.lens(_.players).modify(
            players => players.map(p => p.copy(currentPlayer = p.activePlayer))
          )
          TriggersEvent(makeActivePlayerCurrent, GetPlayerInput[SelectExtraGood])
        } else {
          if (finishedActionPhase) {
            TriggersEvent(gameState, NextRole)
          } else {
            TriggersEvent(nextPlayerState, dispatchAction(role))
          }
        }

        next.t


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

        val filledBuildingSlots = players.map {
          _.colonists.totalWhere {
            case OnBuilding(_) => true
            case _ => false
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


      case CleanupShips =>
        TriggersEvent(
          gameState.lens(_.ships).modify(
            _.map { case (size, ship) =>
              (size, if (ship.isFull) ship.clearShip else ship)
            }
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
        if (gameState.colonistsOnShip == 0) {
          // We already took the colonists off the ship, just let the next player move them
          return TriggersEvent(gameState, GetPlayerInput[SelectColonistMove]).t
        }

        val takeFromSupply = gameState.copy(
          colonistsInSupply = gameState.colonistsInSupply - 1,
          players = players.map(p =>
            if (p.activePlayer) {
              p.copy(colonists = p.colonists.update(InSanJuan, _ + 1))
            } else p
          )
        )

        val colonistsToDistribute = gameState.colonistsOnShip
        val colonistsToEach = players.length / colonistsToDistribute
        val fairColonists = colonistsToEach * players.length
        val leftover = colonistsToDistribute - fairColonists
        val activeSeat = players.indexWhere(_.activePlayer)
        val takesExtra = (0 until leftover).map(i =>
          activeSeat + i
        ).toSet

        val distributedColonists = takeFromSupply.copy(
          colonistsOnShip = 0,
          players = takeFromSupply.players.zipWithIndex.map { case (player, seat) =>
            val extra = if (takesExtra.contains(seat)) 1 else 0
            player.copy(
              colonists = player.colonists.update(InSanJuan, _ + colonistsToEach + extra)
            )
          }
        )

        TriggersEvent(distributedColonists, GetPlayerInput[SelectColonistMove]).t


      case DistributeGoods =>
        val producedGoods = {
          for {
            (good, producable) <- player.goodsProduction.toList
            available = gameState.availableGoods.get(good)
          } yield good -> math.max(available, producable)
          }.toArray

        val allocatedGoods = players.map { p =>
          if (p.currentPlayer) {
            p.copy(
              numberOfGoods = p.numberOfGoods |+| Count(producedGoods: _*)
            )
          } else p
        }

        val nextState = gameState.copy(players = allocatedGoods)
        if (player.colonists.exists(OnBuilding(Factory)))
          TriggersEvent(nextState, PayFactory(producedGoods.length)).t
        else
          TriggersEvent(nextState, NextAction).t


      case PopulateColonistShip =>
        val vacantBuildingSlots = {
          for {
            player <- players
            building <- player.buildings
            occupiedSlots = player.colonists.get(OnBuilding(building))
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

      case DecideToShip(true) =>
        TriggersEvent(gameState, GetPlayerInput[ShipGoods]).t

      case DecideToShip(false) =>
        TriggersEvent(
          gameState.updateCurrentPlayer(_.lens(_.roleState.wharfUsed).set(true)),
          NextAction
        ).t

      case ShipGoods(ship, good) =>
        val playerGoods = player.numberOfGoods.get(good)
        val (goodsAdded, updatedShipState) = ship match {
          case size: ShipSize.PublicShipSize =>
            val currentShip = gameState.ships(size)
            val goodsAdded = math.min(playerGoods, currentShip.remainingCapacity)
            val updatedShips = gameState.ships.updatedWith(size)(_.map(_.addGood(good, goodsAdded)))
            (goodsAdded, gameState.copy(ships = updatedShips))
          case ShipSize.Wharf =>
            val goodsAdded = playerGoods
            val usedWharf = gameState.updateCurrentPlayer(_.lens(_.roleState.wharfUsed).set(true))
            (goodsAdded, usedWharf)
        }

        val harborBonus = if (player.colonists.exists(OnBuilding(Harbor))) 1 else 0
        val captainBonus = if (player.activePlayer && !player.roleState.hasShipped) 1 else 0
        val pointsEarned = goodsAdded + harborBonus + captainBonus

        TriggersEvent(
          updatedShipState.updateCurrentPlayer(
            _.lens(_.points).modify(_ + pointsEarned)
          ).updateCurrentPlayer(
            _.lens(_.numberOfGoods).modify(_.update(good, _ - goodsAdded))
          ),
          NextAction
        ).t

      case StoreGoods(None, _) =>
        TriggersEvent(gameState, NextAction).t

      case StoreGoods(Some(good), usingWarehouse) =>
        val goodCount = if (usingWarehouse)
          player.numberOfGoods.get(good)
        else 1

        val nextState = gameState
          .updateCurrentPlayer(_.lens(_.roleState.storedGoods).modify(_.updated(good, goodCount)))
          .updateCurrentPlayer(_.lens(_.roleState.storedSingleGood).modify(_ || !usingWarehouse))
          .updateCurrentPlayer(_.lens(_.roleState.wharehousesUsed).modify(_ + (if (usingWarehouse) 1 else 0)))

        TriggersEvent(nextState, NextAction).t


      case SellGood(good) =>
        good match {
          case None =>
            TriggersEvent(gameState, NextAction).t
          case Some(soldGood) =>
            val traderBonus = if (player.activePlayer) 1 else 0
            val sMarketBonus = if (player.colonists.exists(OnBuilding(SmallMarket))) 1 else 0
            val lMarketBonus = if (player.colonists.exists(OnBuilding(LargeMarket))) 2 else 0

            val goodWorth = soldGood match {
              case Corn => 0
              case Indigo => 1
              case Sugar => 2
              case Indigo => 3
              case Tobacco => 4
              case Coffee => 5
            }

            val coinsEarned = goodWorth + traderBonus + sMarketBonus + lMarketBonus

            val newState = gameState
              .lens(_.tradingHouse).modify(soldGood :: _)
              .updateCurrentPlayer(_.lens(_.money).modify(_ + coinsEarned))
              .updateCurrentPlayer(_.lens(_.numberOfGoods).modify(_.update(soldGood, _ - 1)))

            val tradingHouseFull = newState.tradingHouse.length == 4

            if (tradingHouseFull)
              TriggersEvent(newState, ClearTradingHouse).t
            else
              TriggersEvent(newState, NextAction).t
        }


      case UseHacienda(use) =>
        val addedPlantation = if (!use) {
          gameState
        } else {
          val (remaining, drawnPlantation) = gameState.hiddenPlantations.takeRandom(gameState.constants.rng)

          gameState.updateCurrentPlayer(_.lens(_.islandTiles).modify(_.update(drawnPlantation, _ + 1)))
            .copy(hiddenPlantations = remaining)
        }
        TriggersEvent(addedPlantation, GetPlayerInput[TakeIslandTile]).t

      case TakeIslandTile(None) =>
        TriggersEvent(gameState, NextAction).t

      case TakeIslandTile(Some(islandTile)) =>
        val updatedPlayer = gameState.updateCurrentPlayer(
          _.lens(_.islandTiles).modify(_.update(islandTile, _ + 1))
            .lens(_.roleState.selectedIslandTile).set(Some(islandTile))
        )

        val removedTile = islandTile match {
          case Quarry => updatedPlayer.lens(_.quarriesRemaining).modify(_ - 1)
          case plantation: Plantation => updatedPlayer.lens(_.shownPlantations).modify(_.update(plantation, _ - 1))
        }

        TriggersEvent(removedTile, GetPlayerInput[UseHospice]).t


      case UseHospice(use) =>
        val selectedTile = player.roleState.selectedIslandTile

        val addedColonist = if (selectedTile.isEmpty || !use)
          gameState
        else {
          gameState.updateCurrentPlayer(
            _.lens(_.colonists).modify(_.update(OnIslandTile(selectedTile.get), _ + 1))
          ).lens(_.colonistsOnShip).modify(_ - 1)
        }

        TriggersEvent(addedColonist, NextAction).t


      case PurchaseBuilding(None) =>
        TriggersEvent(gameState, NextAction).t


      case PurchaseBuilding(Some(building)) =>
        val purchased = gameState.updateCurrentPlayer(
          _.lens(_.buildings).modify(_ + building)
            .lens(_.roleState.purchasedBuilding).set(Some(building))
        ).lens(_.buildingShop).modify(_.update(building, _ - 1))

        TriggersEvent(purchased, GetPlayerInput[UseUniversity]).t


      case UseUniversity(use) =>
        val purchasedBuilding = player.roleState.purchasedBuilding

        val addedColonist = if (purchasedBuilding.isEmpty || !use)
          gameState
        else {
          gameState.updateCurrentPlayer(
            _.lens(_.colonists).modify(_.update(OnBuilding(purchasedBuilding.get), _ + 1))
          ).lens(_.colonistsOnShip).modify(_ - 1)
        }

        TriggersEvent(addedColonist, NextAction).t


      case SelectColonistMove(colonistMovement) =>
        val nextAction = colonistMovement match {
          case ColonistMovement.Add =>
            GetPlayerInput[SelectColonistAddTarget]
          case ColonistMovement.Remove =>
            GetPlayerInput[SelectColonistRemoveTarget]
          case ColonistMovement.Finish =>
            NextAction
        }
        TriggersEvent(gameState, nextAction).t

      case SelectColonistAddTarget(colonistLocation) =>
        val nextState = gameState.updateCurrentPlayer( p =>
          p.copy(colonists =
            p.colonists
              .update(colonistLocation, _ + 1)
              .update(InSanJuan, _ - 1)
          )
        )
        TriggersEvent(nextState, GetPlayerInput[SelectColonistMove]).t


      case SelectColonistRemoveTarget(colonistLocation) =>

        val nextState = gameState.updateCurrentPlayer( p =>
          p.copy(colonists =
            p.colonists
              .update(colonistLocation, _ - 1)
              .update(InSanJuan, _ + 1)
          )
        )
        TriggersEvent(nextState, GetPlayerInput[SelectColonistMove]).t


      case SelectExtraGood(None) =>
        TriggersEvent(gameState, NextRole).t

      case SelectExtraGood(Some(good)) =>
        val takenGood = gameState.updateCurrentPlayer(
          _.lens(_.numberOfGoods).modify(_.update(good, _ + 1))
        ).lens(_.availableGoods).modify(_.update(good, _ - 1))

        TriggersEvent(takenGood, NextRole).t
    }
  }
}
