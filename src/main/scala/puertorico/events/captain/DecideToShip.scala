package puertorico.events.captain

import puertorico.GameState
import puertorico.events.{Enumerable, Event, GetPlayerInput, NextAction}
import monocle.macros.syntax.lens._

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

  override def run(state: GameState): GameState =
    if (!ship) state.updateCurrentPlayer(
      _.lens(_.roleState.wharfUsed).set(true)
    ) else state

  override def nextEvent(state: GameState): Event =
    if (ship) GetPlayerInput[ShipGoods]
    else NextAction
}

object DecideToShip {
  implicit val enum = new Enumerable[DecideToShip] {
    override def allPossibleMoves: Seq[DecideToShip] =
      List(true, false).map(DecideToShip.apply)
  }
}
