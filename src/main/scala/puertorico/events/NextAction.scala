package puertorico.events

import puertorico.components.Role
import puertorico.components.Role._
import puertorico.events.captain.CleanupShips
import puertorico.events.craftsman.SelectExtraGood
import puertorico.events.mayor.PopulateColonistShip
import puertorico.events.settler.RevealNewPlantations
import puertorico.{GameState, components}

object NextAction extends Event {
  override def run(state: GameState): GameState =
    state.copy(currentPlayer = (state.currentPlayer + 1) % state.players.length)

  override def nextEvent(state: GameState): Event = {
    def cleanupRole(role: Role): Event = role match {
      case Captain => CleanupShips
      case Settler => RevealNewPlantations
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
        !state.players.exists(
          p => p.canShipPublic(state.ships.values) || p.canShipWharf
        )
      case _ =>
        state.currentPlayer == state.roleSelector
    }

    if (finishedWithRole) cleanupRole(role) else DispatchRole
  }
}
