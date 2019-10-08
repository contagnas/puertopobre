package puertorico.events.builder

import puertorico.GameState
import puertorico.components.Building
import puertorico.events.{Enumerable, Event, GetPlayerInput}
import monocle.macros.syntax.lens._

case class PurchaseBuilding(purchasedBuilding: Option[Building]) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val largeBuildings = state.currentPlayerState.buildings.count(_.largeBuilding)
    val smallBuildings = state.currentPlayerState.buildings.size - largeBuildings

    val buildingSlotsRemaining = 12 - (largeBuildings * 2 + smallBuildings)
    val canFitSmallBuilding = buildingSlotsRemaining >= 1
    val canFitLargeBuilding = (largeBuildings < 4 && smallBuildings <= 10 - 2 * largeBuildings)
    purchasedBuilding match {
      case Some(building) =>
        val price = building.discountedPrice(state)
        if (state.currentPlayerState.buildings.contains(building))
          Some(s"You may only buy one of each building.")
        else if (price > state.currentPlayerState.money)
          Some(s"You cannot afford $building, it costs $price to you.")
        else if (!state.buildingShop.exists(building))
          Some(s"There are no ${building}s left.")
        else if (building.largeBuilding && !canFitLargeBuilding)
          Some("You do not have room for any more large buildings.")
        else if (!canFitSmallBuilding)
          Some("You do not have room for any more buildings.")
        else None
      case None => None
    }
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
