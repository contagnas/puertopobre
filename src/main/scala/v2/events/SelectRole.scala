package v2.events

import v2.GameState
import v2.components.Role

case class SelectRole(role: Role) extends Event {
  override def validationError(state: GameState): Option[String] =
    if (state.availableRoles.contains(role))
      None
    else
      Some(s"$role is not available")

  override def run(state: GameState): GameState = ???
  override def nextEvent(state: GameState): Event = ???
}
