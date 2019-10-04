package v2.events.builder

import v2.GameState
import v2.components.Building
import v2.events.{Enumerable, Event, GetPlayerInput}
import monocle.macros.syntax.lens._

case class PurchaseBuilding(purchasedBuilding: Option[Building]) extends Event {
  override def validationError(state: GameState): Option[String] =
    purchasedBuilding match {
      case Some(building) =>
        val price = building.discountedPrice(state)
        if (state.currentPlayerState.buildings.contains(building))
          Some(s"You may only buy one of each building.")
        else if (price > state.currentPlayerState.money)
          Some(s"You cannot afford $building, it costs $price to you.")
        else if (!state.buildingShop.exists(building))
          Some(s"There are no ${building}s left.")
        else None
      case None => None
    }

  override def run(state: GameState): GameState = purchasedBuilding match {
    case None => state
    case Some(building) =>
      state.updateCurrentPlayer(
        player => player
          .lens(_.buildings).modify(_ + building)
          .lens(_.roleState.purchasedBuilding).set(Some(building))
          .lens(_.money).modify(_ - building.discountedPrice(state))
      ).lens(_.buildingShop).modify(_.update(building, _ - 1))
  }

  override def nextEvent(state: GameState): Event =
    GetPlayerInput[UseUniversity] // Could skip this if player doesn't have one
}


object PurchaseBuilding {
  implicit val enum = new Enumerable[PurchaseBuilding] {
    override def allPossibleMoves: Seq[PurchaseBuilding] =
      PurchaseBuilding(None) +: Building.values.map(Some.apply).map(PurchaseBuilding.apply)
  }
}
