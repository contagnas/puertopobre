package puertorico.events

import puertorico.GameState
import puertorico.components.Building
import monocle.macros.syntax.lens._
import puertorico.components.Role
import puertorico.events.builder.PurchaseBuilding

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
