package puertorico.events.captain

import monocle.macros.syntax.lens._
import puertorico.GameState
import puertorico.components.Building.Harbor
import puertorico.components.ColonistLocation.ActiveColonist.OnBuilding
import puertorico.components.{Good, ShipSize}
import puertorico.components.ShipSize.{PublicShipSize, Wharf}
import puertorico.components.ShipSize
import puertorico.events.{Enumerable, Event, NextAction}

case class ShipGoods(good: Good, ship: ShipSize) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState
    if (!player.numberOfGoods.exists(good))
      return Some(s"You do not have any $good.")

    ship match {
      case size: PublicShipSize =>
        val ship = state.ships(size)
        if (ship.isFull)
          Some(s"The $size ship is full.")
        else if (!ship.acceptsGood(good))
          Some(s"The $size ship doesn't accept $good, it is already loaded with ${ship.currentGood.get}.")
        else
          None
      case Wharf =>
        if (!player.canShipWharf)
          Some("You do not have a Wharf available.")
        else
          None
    }
  }

  override def run(state: GameState): GameState = {
    val player = state.currentPlayerState
    val playerGoods = player.numberOfGoods.get(good)
    val (goodsAdded, updatedShipState) = ship match {
      case size: ShipSize.PublicShipSize =>
        val currentShip = state.ships(size)
        val goodsAdded = math.min(playerGoods, currentShip.remainingCapacity)
        val updatedShips = state.ships.updatedWith(size)(_.map(_.addGood(good, goodsAdded)))
        (goodsAdded, state.copy(ships = updatedShips))
      case ShipSize.Wharf =>
        val goodsAdded = playerGoods
        val usedWharf = state.updateCurrentPlayer(
          _.lens(_.roleState.wharfUsed).set(true)
        ).copy(availableGoods = state.availableGoods.update(good, _ + playerGoods))
        (goodsAdded, usedWharf)
    }

    val harborBonus = if (player.colonists.exists(OnBuilding(Harbor))) 1 else 0
    val captainBonus = if (state.currentPlayer == state.roleSelector && !player.roleState.hasShipped) 1 else 0
    val pointsEarned = goodsAdded + harborBonus + captainBonus

    updatedShipState.updateCurrentPlayer(
      player => player
        .lens(_.points).modify(_ + pointsEarned)
        .lens(_.numberOfGoods).modify(_.update(good, _ - goodsAdded))
    )
  }

  override def nextEvent(state: GameState): Event = NextAction
}

object ShipGoods {
  implicit val enum = new Enumerable[ShipGoods] {
    override def allPossibleMoves: Seq[ShipGoods] =
      for {
        good <- Good.values
        ship <- ShipSize.values
      } yield ShipGoods(good, ship)
  }
}
