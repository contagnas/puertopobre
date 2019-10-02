package v2.events.captain

import v2.GameState
import v2.components.{Good, ShipSize}
import v2.components.ShipSize.{PublicShipSize, Wharf}
import v2.events.Event

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

  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
