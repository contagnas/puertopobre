import Building._
import ColonistLocation.ActiveColonist._
import ColonistLocation.{ActiveColonist, InSanJuan}
import ColonistMovement.{Add, Finish, Remove}
import Event.{PlayerEvent, SelectColonistAddTarget, SelectColonistRemoveTarget}
import IslandTile.{Plantation, Quarry}
import ShipSize.PublicShipSize

object EventValidator {
  def validate(event: PlayerEvent, gameState: GameState): Either[String, Unit] = {
    val currentPlayer = gameState.players.find(_.currentPlayer).get

    event match {
      case Event.SelectRole(role) =>
        if (gameState.availableRoles.contains(role)) Right(()) else Left(s"$role is not available")

      case Event.DecideToShip(true) =>
        if (currentPlayer.canShipPublic(gameState.ships.values) || currentPlayer.canShipWharf)
          Right(())
        else Left(s"There is nothing you can ship.")

      case Event.DecideToShip(false) =>
        if (currentPlayer.canShipPublic(gameState.ships.values))
          Left("You must ship if able.")
        else Right(())

      case Event.ShipGoods(ship, good) =>
        if (!currentPlayer.numberOfGoods.exists(good))
          return Left(s"You do not have any $good.")

        ship match {
          case size: PublicShipSize =>
            val ship = gameState.ships(size)
            if (ship.isFull)
              Left(s"The $size ship is full.")
            else if (!ship.acceptsGood(good))
              Left(s"The $size ship doesn't accept $good, it is already loaded with ${ship.currentGood.get}.")
            else
              Right(())
          case ShipSize.Wharf =>
             if (!currentPlayer.canShipWharf)
               Left("You do not have a Wharf available.")
             else
               Right(())
        }

      case Event.StoreGoods(None, _) =>
        Right(())

      case Event.StoreGoods(Some(good), usingWarehouse) =>
        val storingPlayer = gameState.players.find(_.currentPlayer).get
        if (storingPlayer.numberOfGoods.exists(good))
          Left(s"You do not have any $good.")
        else if (usingWarehouse && !currentPlayer.warehouseAvailable)
          Left(s"You do not have any warehouses to use.")
        else if (storingPlayer.roleState.storedSingleGood)
          Left(s"You have already stored your single good this round.")
        else Right(())

      case Event.SellGood(good) =>
        if (good.isEmpty) return Right(())

        if (currentPlayer.numberOfGoods.get(good.get) == 0)
          Left(s"You do not have any $good.")
        else if (gameState.tradingHouse.contains(good.get) && !currentPlayer.colonists.exists(OnBuilding(Office)))
          Left(s"A $good has already been sold in the trading house.")
        else Right(())

      case Event.UseHacienda(use) =>
        if (use && !currentPlayer.colonists.exists(OnBuilding(Hacienda)))
          Left(s"You do not have an active Hacienda.")
        else if (use && currentPlayer.islandTiles.total == 9)
          Left(s"You do not have room for any more island tiles")
        else Right(())

      case Event.TakeIslandTile(islandTile) =>
        if (islandTile.isEmpty)
          Right(())
        else if (currentPlayer.islandTiles.total == 9)
          Left(s"You do not have room for any more island tiles")
        else islandTile match {
          case Some(plantation: Plantation) =>
            if (!gameState.shownPlantations.contains(plantation))
              Left(s"There is no $plantation available.")
            else Right(())
          case Some(Quarry) =>
            if (gameState.quarriesRemaining == 0)
              Left(s"There are no Quarries left.")
            else if (!currentPlayer.activePlayer && !currentPlayer.colonists.exists(OnBuilding(ConstructionHut)))
              Left(s"You may not take a Quarry. Take one of the plantations.")
            else Right(())
        }

      case Event.UseHospice(use) =>
        if (use && !currentPlayer.colonists.exists(OnBuilding(Hospice)))
          Left(s"You do not have an active Hospice")
        else Right(())

      case Event.PurchaseBuilding(building) =>
        building match {
          case Some(build) =>
            val price = build.discountedPrice(currentPlayer)
            if (price > currentPlayer.money)
              Left(s"You cannot afford $build, it costs $price to you.")
            else if (!gameState.buildingShop.exists(build))
              Left(s"There are no ${build}s left.")
            else Right(())
          case None => Right(())
        }

      case Event.UseUniversity(use) =>
        if (use && !currentPlayer.colonists.exists(OnBuilding(University)))
          Left(s"You do not have an active University")
        else Right(())


      case Event.SelectColonistMove(Finish) =>
        val colonists = currentPlayer.colonists
        val possibleSlots = currentPlayer.islandTiles.totalWhere(_ => true) + currentPlayer.buildings.map(_.colonistSlots).sum
        if (possibleSlots >= colonists.total && colonists.get(InSanJuan) > 0)
          Left(s"You may not keep colonists on San Juan when there are empty spaces on your board.")
        else Right(())

      case Event.SelectColonistMove(Add) =>
        if (!currentPlayer.colonists.exists(InSanJuan))
          Left(s"You have no colonists in San Juan to add.")
        else Right(())

      case Event.SelectColonistMove(Remove) =>
        val activeColonists = currentPlayer.colonists.totalWhere {
          case _: ActiveColonist => true
          case _ => false
        }

        if (activeColonists > 0) Right(())
        else Left("You do not have any active colonists to remove.")



      case SelectColonistRemoveTarget(target) =>
        if (currentPlayer.colonists.get(target) > 0)
          Right(())
        else Left(s"You do not have any colonists $target")


      case SelectColonistAddTarget(target) =>
        target match {
          case OnBuilding(building) =>
            if (!currentPlayer.buildings.contains(building))
              Left(s"You do not have a $building.")
            else if (building.colonistSlots <= currentPlayer.colonists.get(OnBuilding(building)))
              Left(s"$building is already fully occupied.")
            else
              Right(())
          case OnIslandTile(islandTile) =>
            if (currentPlayer.islandTiles.get(islandTile) > currentPlayer.colonists.get(OnIslandTile(islandTile)))
              Right(())
            else
              Left(s"You do not have an unoccupied $islandTile.")
        }

      case Event.SelectExtraGood(None) =>
        Right(())

      case Event.SelectExtraGood(Some(good)) =>
        if (!currentPlayer.goodsProduction.contains(good))
          Left(s"You may only select a good you can produce.")
        else if (!gameState.availableGoods.exists(good))
          Left(s"There is no more $good available")
        else Right(())
    }
  }
}
