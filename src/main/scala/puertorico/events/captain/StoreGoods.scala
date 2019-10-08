package puertorico.events.captain

import puertorico.events.{Event, NextAction}
import puertorico.{GameState, Player}
import monocle.macros.syntax.lens._
import puertorico.components.Good

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

  override def run(state: GameState): GameState = goodToStore match {
    case None => state
    case Some(good) =>
      val goodCount = if (usingWarehouse)
        state.currentPlayerState.numberOfGoods.get(good)
      else
        1

      state
        .updateCurrentPlayer(_.lens(_.roleState.storedGoods).modify(_.updated(good, goodCount)))
        .updateCurrentPlayer(_.lens(_.roleState.storedSingleGood).modify(_ || !usingWarehouse))
        .updateCurrentPlayer(_.lens(_.roleState.warehousesUsed).modify(_ + (if (usingWarehouse) 1 else 0)))
  }

  override def nextEvent(state: GameState): Event = NextAction
}
