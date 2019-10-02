package v2.events.captain

import v2.components.Good
import v2.events.{Event, NextAction}
import v2.{GameState, Player}

case class StoreGoods(goodToStore: Option[Good], usingWarehouse: Boolean) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player: Player = state.currentPlayerState

    val playerHasGood: Either[String, Unit] =
      Some(s"You do not have any $goodToStore")
        .filter(_ => goodToStore.forall(player.numberOfGoods.exists))
        .toLeft(())

    val playerHasWarehouseSpace =
      Some(s"You do not have any warehouse space left")
        .filter(_ => player.warehouseAvailable)
        .toLeft(())

    val playerCanStoreSingleGood: Either[String, Unit] =
      Some("You have already used your non-warehouse storage for this role..")
        .filter(_ => !player.roleState.storedSingleGood)
        .toLeft(())

    (goodToStore, usingWarehouse) match {
      case (None, false) => None // Skip / finish
      case (None, true) => Some("You can't store nothing and use a warehouse.")
      case (Some(_), false) =>
        (for {
          _ <- playerHasGood
          singleStorageAvailable <- playerCanStoreSingleGood
        } yield singleStorageAvailable).swap.toOption
      case (Some(_), true) =>
        (for {
          _ <- playerHasGood
          hasWarehouse <- playerHasWarehouseSpace
        } yield hasWarehouse).swap.toOption
    }
  }

  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event = NextAction
}
