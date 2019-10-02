package v2.events

import v2.GameState
import v2.components.Role._
import v2.events.builder.PurchaseBuilding
import v2.events.captain.DecideToShip
import v2.events.craftsman.DistributeGoods
import v2.events.mayor.DistributeColonists
import v2.events.prospector.PayProspector
import v2.events.settler.UseHacienda
import v2.events.trader.SellGood

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
