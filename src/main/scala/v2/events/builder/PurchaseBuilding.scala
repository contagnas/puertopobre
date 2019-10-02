package v2.events.builder

import v2.GameState
import v2.components.Building
import v2.events.{Event, GetPlayerInput}

case class PurchaseBuilding(building: Option[Building]) extends Event {
  override def validationError(state: GameState): Option[String] = building match {
    case Some(build) =>
      val price = build.discountedPrice(state)
      if (price > state.currentPlayerState.money)
        Some(s"You cannot afford $build, it costs $price to you.")
      else if (!state.buildingShop.exists(build))
        Some(s"There are no ${build}s left.")
      else None
    case None => None
  }

  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event =
    GetPlayerInput[UseUniversity] // Could skip this if player doesn't have one
}
