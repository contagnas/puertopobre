import Building._
import ColonistLocation.{OnBuilding, OnIslandTile, InSanJuan}
import Event.PlayerEvent
import IslandTile.{Plantation, Quarry}
import ShipSize.PublicShipSize

object EventValidator {
  def validate(event: PlayerEvent, gameState: GameState): Either[String, Unit] = {
    val currentPlayer = gameState.players.find(_.currentPlayer).get

    event match {
      case Event.SelectRole(role) =>
        if (gameState.availableRoles.contains(role)) Right(()) else Left(s"$role is not available")

      case Event.ShipGoods(ship, good) =>
        val shippingPlayer = gameState.players.find(_.roleState.shippingGoods).get
         ship match {
          case size: PublicShipSize =>
            val ship = gameState.ships(size)
            if (shippingPlayer.numberOfGoods(good) == 0)
              Left(s"You do not have any $good.")
            else if (ship.isFull)
              Left(s"The $size ship is full.")
            else if (!ship.acceptsGood(good))
              Left(s"The $size ship doesn't accept $good, it is already loaded with ${ship.currentGood.get}.")
            else
              Right(())
          case ShipSize.Wharf =>
             if (!shippingPlayer.roleState.wharfAvailable)
               Left("You do not have a Wharf available.")
             else
               Right(())
        }

      case Event.StoreGoods(good, usingWarehouse) =>
        val storingPlayer = gameState.players.find(_.roleState.storingGoods).get
        if (storingPlayer.numberOfGoods(good) == 0)
          Left(s"You do not have any $good.")
        else if (usingWarehouse && currentPlayer.roleState.warehouseUses == 0)
          Left(s"You do not have any warehouses to use.")
        else Right(())

      case Event.SellGood(good) =>
        if (good.isEmpty) return Right(())

        if (currentPlayer.numberOfGoods(good.get) == 0)
          Left(s"You do not have any $good.")
        else if (gameState.tradingHouse.contains(good.get) && currentPlayer.colonists(OnBuilding(Office)) > 0)
          Left(s"A $good has already been sold in the trading house.")
        else Right(())

      case Event.UseHacienda(use) =>
        if (use && currentPlayer.colonists(OnBuilding(Hacienda)) > 0)
          Left(s"You do not have an active Hacienda.")
        else Right(())

      case Event.TakeIslandTisle(islandTile) =>
        islandTile match {
          case plantation: Plantation =>
            if (!gameState.shownPlantations.contains(plantation))
              Left(s"There is no $plantation available.")
            else Right(())
          case Quarry =>
            if (gameState.quarriesRemaining == 0)
              Left(s"There are no Quarries left.")
            else if (!currentPlayer.activePlayer || currentPlayer.colonists(OnBuilding(ConstructionHut)) == 0)
              Left(s"You may not take a Quarry. Take one of the plantations.")
            else Right(())
        }

      case Event.UseHospice(use) =>
        if (use && currentPlayer.colonists(OnBuilding(Hospice)) == 0)
          Left(s"You do not have an active Hospice")
        else Right(())

      case Event.PurchaseBuilding(building) =>
        building match {
          case Some(build) =>
            val activeDiscount = if (currentPlayer.activePlayer) 1 else 0
            val quarries = currentPlayer.colonists(OnIslandTile(Quarry))
            val quarryDiscount = math.min(quarries, build.maxQuarries)
            val price = build.price - activeDiscount - quarryDiscount
            if (price > currentPlayer.money)
              Left(s"You cannot afford $build, it costs $price to you.")
            else if (gameState.buildingShop(build) < 1)
              Left(s"There are no ${build}s left.")
            else Right(())
          case None => Right(())
        }

      case Event.UseUniversity(use) =>
        if (use && currentPlayer.colonists(OnBuilding(University)) == 0)
          Left(s"You do not have an active University")
        else Right(())

      case Event.RearrangeColonists(newArrangement) =>
        val player = gameState.players.find(_.roleState.colonistPlacingPlayer).get
        val possibleSlots = player.islandTiles.values.sum + player.buildings.map(_.colonistSlots).sum
        if (possibleSlots >= newArrangement.values.sum && newArrangement(InSanJuan) > 0)
          Left(s"You may not place colonists on San Juan when there are empty spaces on your board.")
        else if (player.colonists.values.sum != newArrangement.values.sum)
          Left(s"The new colonist arrangement must have the same number as the previous arrangement.")
        else Right(())

      case Event.SelectExtraGood(good) =>
        if (!currentPlayer.goodsProduction.contains(good))
          Left(s"You may only select a good you can produce.")
        else if (gameState.availableGoods(good) == 0)
          Left(s"There is no more $good available")
        else Right(())
    }
  }
}
