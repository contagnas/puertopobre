package v2.events.mayor

import v2.GameState
import v2.events.{Event, NextRole}
import v2.components.ColonistLocation.ActiveColonist.OnBuilding

object PopulateColonistShip extends Event {
  override def run(state: GameState): GameState = {

    val vacantBuildingSlots = {
      for {
        player <- state.players
        building <- player.buildings
        occupiedSlots = player.colonists.get(OnBuilding(building))
      } yield building.colonistSlots - occupiedSlots
      }.sum

    val numberToPopulate = math.max(vacantBuildingSlots, state.constants.minimumColonistsOnShip)

    state.copy(
      colonistsOnShip = math.min(numberToPopulate, state.colonistsInSupply),
      colonistsInSupply = math.max(0, state.colonistsInSupply - numberToPopulate)
    )
  }

  override def nextEvent(state: GameState): Event = NextRole
}
