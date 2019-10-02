package v2.events.captain

import v2.GameState
import v2.events.Event

case class DecideToShip(ship: Boolean) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState
    val canShipPublic = player.canShipPublic(state.ships.values)
    val canShipWharf = player.canShipWharf

    if (ship && !canShipPublic && !canShipWharf)
      Some(s"There is nothing you can ship.")
    else if (!ship && canShipPublic)
      Some("You must ship if able.")
    else
      None
  }

  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event = ???
}
