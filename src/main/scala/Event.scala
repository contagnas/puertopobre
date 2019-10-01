import ColonistLocation.ActiveColonist
import ColonistLocation.ActiveColonist.{OnBuilding, OnIslandTile}
import ShipSize.PublicShipSize
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
  case object CleanupShips extends GameEvent
  case object ClearTradingHouse extends GameEvent
  case object RevealNewPlantations extends GameEvent
  case object DistributeColonists extends GameEvent
  case object DistributeGoods extends GameEvent
  case object PopulateColonistShip extends GameEvent
  case object PayProspector extends GameEvent
  case class PayFactory(numberOfGoods: Int) extends GameEvent

  trait ParseableEvent[T <: PlayerEvent] {
    def prompt: List[String]
    def parse(input: List[String]): Either[String, T]
  }

  object ParseableEvent {
    private val truthy = Set("yes", "y", "true", "ok")
    private val falsey = Set("no", "n", "false")
    private def stringToBool(string: String): Either[String, Boolean] = {
      if (truthy.contains(string.strip.toLowerCase))
        Right(true)
      else if (falsey.contains(string.strip.toLowerCase))
        Right(false)
      else
        Left(s"$string is not a valid input, use [yes|no]")
    }

    private def enumHint[T <: EnumEntry](enum: Enum[T]): String =
      enum.namesToValuesMap.keys.mkString("[","|","]")

    private def parseEnum[T <: EnumEntry](string: String, enum: Enum[T]): Either[String, T] = {
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

    implicit val parsePurchaseBuilding = new ParseableEvent[PurchaseBuilding] {
      override def prompt: List[String] = List(
        s"Select a building (or 'none' to buy nothing): ${enumHint(Building)}"
      )

      override def parse(input: List[String]): Either[String, PurchaseBuilding] =
        input.head.toLowerCase match {
          case "none" => Right(PurchaseBuilding(None))
          case _ => parseEnum(input.head, Building).map(b => PurchaseBuilding(Some(b)))
        }
    }

    implicit val useUniversity = new ParseableEvent[UseUniversity] {
      override def prompt: List[String] = List(
        "Use a university?"
      )

      override def parse(input: List[String]): Either[String, UseUniversity] =
        stringToBool(input.head).map(UseUniversity.apply)
    }

    implicit val useHospice = new ParseableEvent[UseHospice] {
      override def prompt: List[String] = List(
        "Use a hospice?"
      )

      override def parse(input: List[String]): Either[String, UseHospice] =
        stringToBool(input.head).map(UseHospice.apply)
    }

    implicit val useHacienda = new ParseableEvent[UseHacienda] {
      override def prompt: List[String] = List(
        "Use an hacienda?"
      )

      override def parse(input: List[String]): Either[String, UseHacienda] =
        stringToBool(input.head).map(UseHacienda.apply)
    }

    implicit val takeIslandTile = new ParseableEvent[TakeIslandTile] {
      override def prompt: List[String] = List(
        s"Select a tile (or 'none' to take nothing): ${enumHint(IslandTile)}"
      )

      override def parse(input: List[String]): Either[String, TakeIslandTile] =
        input.head.toLowerCase match {
          case "none" => Right(TakeIslandTile(None))
          case _ => parseEnum(input.head, IslandTile).map(b => TakeIslandTile(Some(b)))
        }
    }

    implicit val parseStoreGoods = new ParseableEvent[StoreGoods] {
      override def prompt: List[String] = List(
        s"Select a good to store ('none' to store nothing) ${enumHint(Good)}",
        s"Use a warehouse?"
      )

      override def parse(input: List[String]): Either[String, StoreGoods] = {
        val List(good, useWarehouse) = input
        for {
          g <- if ("none" == good) Right(None) else parseEnum(good, Good).map(Some.apply)
          w <- stringToBool(useWarehouse)
        } yield StoreGoods(g, w)
      }
    }

    implicit val parseColAddTarg = new ParseableEvent[SelectColonistAddTarget] {
      override def prompt: List[String] = List(
        s"Add a colonist to where? ${enumHint(IslandTile)} or ${enumHint(Building)}"
      )

      override def parse(input: List[String]): Either[String, SelectColonistAddTarget] = {
        val targetBuilding = parseEnum(input.head, Building)
        val targetIsland = parseEnum(input.head, IslandTile)

        (targetBuilding, targetIsland) match {
          case (Left(_), Right(island)) => Right(SelectColonistAddTarget(OnIslandTile(island)))
          case (Right(building), Left(_)) => Right(SelectColonistAddTarget(OnBuilding(building)))
          case (Left(error1), Left(error2)) => Left(List(error1, error2).mkString("\n"))
          case (Right(_), Right(_)) => throw new IllegalStateException(s"$input parsed as both island and building")
        }
      }
    }

    implicit val parseColRemoveTarg = new ParseableEvent[SelectColonistRemoveTarget] {
      override def prompt: List[String] = List(
        s"Add a colonist to where? ${enumHint(IslandTile)} or ${enumHint(Building)}"
      )

      override def parse(input: List[String]): Either[String, SelectColonistRemoveTarget] = {
        val targetBuilding = parseEnum(input.head, Building)
        val targetIsland = parseEnum(input.head, IslandTile)

        (targetBuilding, targetIsland) match {
          case (Left(_), Right(island)) => Right(SelectColonistRemoveTarget(OnIslandTile(island)))
          case (Right(building), Left(_)) => Right(SelectColonistRemoveTarget(OnBuilding(building)))
          case (Left(error1), Left(error2)) => Left(List(error1, error2).mkString("\n"))
          case (Right(_), Right(_)) => throw new IllegalStateException(s"$input parsed as both island and building")
        }
      }
    }

    implicit val parseSelectColMove = new ParseableEvent[SelectColonistMove] {
      override def prompt: List[String] = List(
        s"Select colonist move: ${enumHint(ColonistMovement)}"
      )

      override def parse(input: List[String]): Either[String, SelectColonistMove] =
        parseEnum(input.head, ColonistMovement).map(SelectColonistMove)
    }

    implicit val parseExtraGood = new ParseableEvent[SelectExtraGood] {
      override def prompt: List[String] = List(
        s"Select extra good: ${enumHint(Good)}"
      )

      override def parse(input: List[String]): Either[String, SelectExtraGood] = {
        input.head.toLowerCase match {
          case "none" => Right(None)
          case _ => parseEnum(input.head, Good).map(Some.apply)
        }
      }.map(SelectExtraGood.apply)
    }

    implicit val parseSellGood = new ParseableEvent[SellGood] {
      override def prompt: List[String] = List(
        s"Select good to sell: ${enumHint(Good)}"
      )

      override def parse(input: List[String]): Either[String, SellGood] = {
        input.head.toLowerCase match {
          case "none" => Right(None)
          case _ => parseEnum(input.head, Good).map(Some.apply)
        }
        }.map(SellGood.apply)
    }

    implicit val parseDecideToShip = new ParseableEvent[DecideToShip] {
      override def prompt: List[String] = "Do you want to ship?" :: Nil

      override def parse(input: List[String]): Either[String, DecideToShip] =
        stringToBool(input.head).map(DecideToShip.apply)
    }
  }

  case class GetPlayerInput[T <: PlayerEvent]()
    (implicit val parser: ParseableEvent[T]) extends IOEvent

  case class SelectRole(role: Role) extends PlayerEvent

  // Captain
  case class DecideToShip(ship: Boolean) extends PlayerEvent
  case class ShipGoods(ship: ShipSize, good: Good) extends PlayerEvent
  case class StoreGoods(good: Option[Good], usingWarehouse: Boolean) extends PlayerEvent
  // Trader
  case class SellGood(good: Option[Good]) extends PlayerEvent
  // Settler
  case class UseHacienda(use: Boolean) extends PlayerEvent
  case class TakeIslandTile(islandTile: Option[IslandTile]) extends PlayerEvent
  case class UseHospice(use: Boolean) extends PlayerEvent
  // Builder
  case class PurchaseBuilding(building: Option[Building]) extends PlayerEvent
  case class UseUniversity(use: Boolean) extends PlayerEvent
  // Mayor
  case class SelectColonistMove(colonistMovement: ColonistMovement) extends PlayerEvent
  case class SelectColonistAddTarget(colonistLocation: ActiveColonist) extends PlayerEvent
  case class SelectColonistRemoveTarget(colonistLocation: ActiveColonist) extends PlayerEvent
  // Craftsman
  case class SelectExtraGood(good: Option[Good]) extends PlayerEvent

}
