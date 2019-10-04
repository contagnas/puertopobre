package v2.events.builder

import monocle.macros.syntax.lens._
import v2.GameState
import v2.components.Building.University
import v2.components.ColonistLocation.ActiveColonist.OnBuilding
import v2.events.{Enumerable, Event, NextAction}

case class UseUniversity(use: Boolean) extends Event {
  override def validationError(state: GameState): Option[String] =
    Some(s"You do not have an active University")
      .filter(_ => use)
      .filterNot(_ => state.currentPlayerState.colonists.exists(OnBuilding(University)))

  override def run(state: GameState): GameState =
    if (!use) state else {
      state.currentPlayerState.roleState.purchasedBuilding match {
        case None => state
        case Some(building) =>
          state.updateCurrentPlayer(
            _.lens(_.colonists).modify(_.update(OnBuilding(building), _+ 1))
          ).lens(_.colonistsInSupply).modify(_ - 1)
      }
    }

  override def nextEvent(state: GameState): Event = NextAction
}

object UseUniversity {
  implicit val enum = new Enumerable[UseUniversity] {
    override def allPossibleMoves: Seq[UseUniversity] =
      List(true, false).map(UseUniversity.apply)
  }
}
