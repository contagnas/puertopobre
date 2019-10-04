package v2.events.captain

import cats.implicits._
import v2.components.{Good, Ship, ShipSize}
import v2.events.{Event, NextRole}
import v2.{Count, GameState}

object CleanupShips extends Event {
  override def run(state: GameState): GameState = {
  val clearedShipsAndGoods: Seq[(ShipSize.PublicShipSize, (Count[Good], Ship))] =
    state.ships.toList.map { case (size, ship) =>
      if (ship.isFull)
        size -> ship.clearShip
      else
        size -> (Count.empty[Good] -> ship)
    }

  val clearedShips = clearedShipsAndGoods.map { case (size, (_, ship)) => size -> ship }.toMap
  val clearedGoods = clearedShipsAndGoods.map { case (_, (goods, _)) => goods }
    .foldLeft(Count.empty[Good])(_ |+| _)

    state.copy(
      ships = clearedShips,
      availableGoods = state.availableGoods |+| clearedGoods
    )
  }

  override def nextEvent(state: GameState): Event = NextRole
}
