package puertorico.events

import puertorico.GameState
import puertorico.components.Role._
import puertorico.events.builder.PurchaseBuilding
import puertorico.events.captain.DecideToShip
import puertorico.events.craftsman.DistributeGoods
import puertorico.events.mayor.DistributeColonists
import puertorico.events.prospector.PayProspector
import puertorico.events.settler.UseHacienda
import puertorico.events.trader.SellGood

object DispatchRole extends Event {
  override def run(state: GameState): GameState = state

  override def nextEvent(state: GameState): Event =
    state.currentRole match {
      case Some(Captain) => GetPlayerInput[DecideToShip]
      case Some(Settler) => GetPlayerInput[UseHacienda]
      case Some(Builder) => GetPlayerInput[PurchaseBuilding]
      case Some(Trader) => GetPlayerInput[SellGood]
      case Some(Mayor) => DistributeColonists
      case Some(Craftsman) => DistributeGoods
      case Some(Prospector) => PayProspector
      case Some(Prospector2) => PayProspector
      case None => throw new IllegalArgumentException(
        "Dispatch role with no role"
      )
    }
}

