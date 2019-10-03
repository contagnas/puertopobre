package v2.events.mayor

import enumeratum._
import v2.GameState
import v2.components.ColonistLocation.{ActiveColonist, InSanJuan}
import v2.events.{Enumerable, Event, GetPlayerInput, NextAction}

sealed trait ColonistMove extends EnumEntry

object ColonistMove extends Enum[ColonistMove] {
  override def values: IndexedSeq[ColonistMove] = findValues
  case object Finish extends ColonistMove
  case object Add extends ColonistMove
  case object Remove extends ColonistMove
}

case class SelectColonistMove(colonistMove: ColonistMove) extends Event {
  import v2.events.mayor.ColonistMove._
  override def validationError(state: GameState): Option[String] = {
    val player = state.currentPlayerState
    val activeColonists = player.colonists.totalWhere {
      case _: ActiveColonist => true
      case _ => false
    }

    val colonistSlots = (
      player.islandTiles.totalWhere(_ => true) +
        player.buildings.map(_.colonistSlots).sum
    )

    colonistMove match {
      case Remove =>
        if (player.colonists.exists(InSanJuan))
          Some("You already have colonists in San Juan, place them before removing others.")
        else if (activeColonists == 0)
          Some("You do not have any active colonists to remove.")
        else None

      case Add =>
        if (!player.colonists.exists(InSanJuan))
          Some(s"You have no colonists in San Juan to add.")
        else if (activeColonists == colonistSlots)
          Some(s"You already have colonists in all possible spots.")
        else None

      case Finish =>
        val colonists = player.colonists
        val allowedInSanJuan = math.max(0, colonists.total - colonistSlots)

        if (colonists.get(InSanJuan) != allowedInSanJuan)
          Some(s"You may not keep colonists on San Juan when there are empty spaces on your board.")
        else None
    }
  }

  override def run(state: GameState): GameState = state

  override def nextEvent(state: GameState): Event = colonistMove match {
    case Remove => GetPlayerInput[SelectColonistRemoveTarget]
    case Add => GetPlayerInput[SelectColonistAddTarget]
    case Finish => NextAction
  }
}

object SelectColonistMove {
  implicit val enum = new Enumerable[SelectColonistMove] {
    override def allPossibleMoves: Seq[SelectColonistMove] =
      ColonistMove.values.map(SelectColonistMove.apply)
  }
}