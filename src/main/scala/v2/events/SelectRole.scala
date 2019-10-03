package v2.events

import v2.GameState
import v2.components.{Building, Role}
import monocle.macros.syntax.lens._
import v2.events.builder.PurchaseBuilding

case class SelectRole(role: Role) extends Event {
  override def validationError(state: GameState): Option[String] =
    Some(s"$role is not available.")
      .filterNot(_ => state.availableRoles.contains(role))

  override def run(state: GameState): GameState = {
    val incentivePayout = state.roleIncentives.get(role)

    state
      .updateCurrentPlayer(_.lens(_.money).modify(_ + incentivePayout))
      .lens(_.currentRole).set(Some(role))
      .lens(_.availableRoles).modify(_ - role)
      .lens(_.roleIncentives).modify(_.set(role, 0))
  }

  override def nextEvent(state: GameState): Event = DispatchRole
}

object SelectRole {
  implicit val enum = new Enumerable[SelectRole] {
    override def allPossibleMoves: Seq[SelectRole] =
      Role.values.map(SelectRole.apply)
  }
}
