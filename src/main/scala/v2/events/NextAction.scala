package v2.events

import v2.components.Role._
import v2.events.captain.CleanupShips
import v2.events.craftsman.SelectExtraGood
import v2.events.mayor.PopulateColonistShip
import v2.events.settler.RevealNewPlantations
import v2.{GameState, components}

object NextAction extends Event {
  override def run(state: GameState): GameState = ???

  override def nextEvent(state: GameState): Event = {
    def cleanupRole(role: components.Role): Event = role match {
      case Captain => CleanupShips
      case Craftsman => RevealNewPlantations
      case Mayor => PopulateColonistShip
      case Craftsman => GetPlayerInput[SelectExtraGood]
      case _ => NextRole
    }

    val role = state.currentRole.getOrElse(
      throw new IllegalStateException("Next action with no role?")
    )

    val finishedWithRole = role match {
      case Captain =>
        // Captain is special because multiple rounds of actions can occur
        state.players.exists(
          p => p.canShipPublic(state.ships.values) || p.canShipWharf
        )
      case _ =>
        state.currentPlayer == state.roleSelector
    }

    if (finishedWithRole) cleanupRole(role) else DispatchRole
  }
}
