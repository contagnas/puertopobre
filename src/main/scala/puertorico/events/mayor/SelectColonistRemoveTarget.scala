package puertorico.events.mayor

import monocle.macros.syntax.lens._
import puertorico.GameState
import puertorico.components.ColonistLocation.{ActiveColonist, InSanJuan}
import puertorico.events.{Enumerable, Event, GetPlayerInput}

case class SelectColonistRemoveTarget(removeTarget: ActiveColonist) extends Event {
  override def validationError(state: GameState): Option[String] =
    if (state.currentPlayerState.colonists.get(removeTarget) == 0)
      Some(s"You do not have any colonists $removeTarget")
    else None

  override def run(state: GameState): GameState = state.updateCurrentPlayer(
    player => player
      .lens(_.colonists).modify(_.update(removeTarget, _ - 1))
      .lens(_.colonists).modify(_.update(InSanJuan, _ + 1))
  )

  override def nextEvent(state: GameState): Event =
    GetPlayerInput[SelectColonistMove]
}

object SelectColonistRemoveTarget {
  implicit val enum = new Enumerable[SelectColonistRemoveTarget] {
    override def allPossibleMoves: Seq[SelectColonistRemoveTarget] =
      ActiveColonist.values.map(SelectColonistRemoveTarget.apply)
  }
}
