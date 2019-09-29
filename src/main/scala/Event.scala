import Building.{Warehouse, Wharf}
import Good._
import enumeratum._
import ShipSize.{PublicShipSize}
import enumeratum._

sealed trait Event { self =>
  def render() = println(self)
}

object Event {
  sealed trait GameEvent extends Event
  sealed trait PlayerEvent extends Event
  sealed trait IOEvent extends Event

  case object NextRound extends GameEvent
  case object NextRole extends GameEvent
  case object NextAction extends GameEvent
  case object NextShipper extends GameEvent
  case object GameOver extends GameEvent
  case object RefillColonists extends GameEvent
  case class CleanUpShip(ship: PublicShipSize) extends GameEvent
  case object ClearTradingHouse extends GameEvent
  case object RevealNewPlantations extends GameEvent
  case object DistributeColonists extends GameEvent
  case object DistributeGoods extends GameEvent
  case object PopulateColonistShip extends GameEvent
  case class PayFactory(numberOfGoods: Int) extends GameEvent

  trait ParseableEvent[T <: PlayerEvent] {
    def prompt: List[String]
    def parse(input: List[String]): Either[String, T]
  }

  object ParseableEvent {
    def enumHint[T <: EnumEntry](enum: Enum[T]): String =
      enum.namesToValuesMap.keys.mkString("[","|","]")

    def parseEnum[T <: EnumEntry](string: String, enum: Enum[T]): Either[String, T] = {
      enum.withNameInsensitiveOption(string)
        .toRight(s"Invalid $enum: [$string]. Use one of ${enumHint(enum)}")
    }

    implicit val parseShipGoods: ParseableEvent[ShipGoods] = new ParseableEvent[ShipGoods] {
      override def prompt: List[String] = List(
        s"Ship to use ${enumHint(ShipSize)}:",
        s"Good to ship ${enumHint(ShipSize)}"
      )

      override def parse(input: List[String]): Either[String, ShipGoods] = {
        val List(size, good) = input

        for {
          shipSize <- parseEnum(size, ShipSize)
          goodType <- parseEnum(good, Good)
        } yield ShipGoods(shipSize, goodType)
      }
    }

    implicit val parseSelectRole = new ParseableEvent[SelectRole] {
      override def prompt: List[String] = List(
        s"Select a role ${enumHint(Role)}"
      )

      override def parse(input: List[String]): Either[String, SelectRole] =
        parseEnum(input.head, Role).map(SelectRole.apply)
    }
  }

  case class GetPlayerInput[T <: PlayerEvent]()
    (implicit val parser: ParseableEvent[T]) extends IOEvent

  case class SelectRole(role: Role) extends PlayerEvent

  // Captain
  case class ShipGoods(ship: ShipSize, good: Good) extends PlayerEvent
  case class StoreGoods(good: Good, usingWarehouse: Boolean) extends PlayerEvent
  // Trader
  case class SellGood(good: Option[Good]) extends PlayerEvent
  // Settler
  case class UseHacienda(use: Boolean) extends PlayerEvent
  case class TakeIslandTisle(plantation: IslandTile) extends PlayerEvent
  case class UseHospice(use: Boolean) extends PlayerEvent
  // Builder
  case class PurchaseBuilding(building: Option[Building]) extends PlayerEvent
  case class UseUniversity(use: Boolean) extends PlayerEvent
  // Mayor
  case class RearrangeColonists(newArrangement: Map[ColonistLocation, Int]) extends PlayerEvent
  // Craftsman
  case class SelectExtraGood(good: Good) extends PlayerEvent

}






