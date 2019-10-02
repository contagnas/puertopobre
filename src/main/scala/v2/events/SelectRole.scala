package v2.events

import v2.GameState
import v2.components.Role

case class SelectRole(role: Role) extends Event {
  override def validationError(state: GameState): Option[String] =
    Some(s"$role is not available.")
      .filterNot(_ => state.availableRoles.contains(role))

  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event = DispatchRole
}
