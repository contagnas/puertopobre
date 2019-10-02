package v2.events.mayor

import v2.components.ColonistLocation.{ActiveColonist, InSanJuan}
import enumeratum._
import v2.GameState
import v2.events.{Event, GetPlayerInput, NextAction}
import v2.events.mayor.ColonistMove._

sealed trait ColonistMove extends EnumEntry

object ColonistMove extends Enum[ColonistMove] {
  override def values: IndexedSeq[ColonistMove] = findValues
  object Finish extends ColonistMove
  object Add extends ColonistMove
  object Remove extends ColonistMove
}

case class SelectColonistMove(colonistMove: ColonistMove) extends Event {
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState
    colonistMove match {
      case Remove =>
        val activeColonists = player.colonists.totalWhere {
          case _: ActiveColonist => true
          case _ => false
        }

        Some("You do not have any active colonists to remove.")
          .filter(_ => activeColonists > 0)

      case Add =>
        Some(s"You have no colonists in San Juan to add.")
          .filter(_ => player.colonists.exists(InSanJuan))

      case Finish =>
        val colonists = player.colonists
        val possibleSlots = player.islandTiles.totalWhere(_ => true) + player.buildings.map(_.colonistSlots).sum
        Some(s"You may not keep colonists on San Juan when there are empty spaces on your board.")
          .filter(_ => colonists.exists(InSanJuan))
          .filter(_ => possibleSlots >= colonists.total)
    }
  }

  override def run(state: GameState): GameState = state

  override def nextEvent(state: GameState): Event = colonistMove match {
    case Remove => GetPlayerInput[SelectColonistRemoveTarget]
    case Add => GetPlayerInput[SelectColonistAddTarget]
    case Finish => NextAction
  }
}
