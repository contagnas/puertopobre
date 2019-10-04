package v2.components

import enumeratum._
import v2.Count

class Ship private (val size: Int, val currentGood: Option[Good], val count: Int) {
  val remainingCapacity: Int = size - count
  val isFull: Boolean = remainingCapacity == 0
  def acceptsGood(good: Good): Boolean =
    !isFull && currentGood.forall(_ == good)

  def addGood(goodToAdd: Good, countToAdd: Int): Ship =
    new Ship(size, Some(goodToAdd), count + countToAdd)

  def clearShip: (Count[Good], Ship) = {
    val clearedGoods: Count[Good] = if (currentGood.isEmpty)
      Count.empty
    else
      Count(currentGood.get -> count)

    clearedGoods -> new Ship(size, None, 0)
  }
}

object Ship {
  def makeShip(size: Int) = new Ship(size, None, 0)
}

sealed trait ShipSize extends EnumEntry

object ShipSize extends Enum[ShipSize] {
  sealed trait PublicShipSize extends ShipSize with EnumEntry

  object PublicShipSize extends Enum[ShipSize] {
    case object Small extends PublicShipSize
    case object Medium extends PublicShipSize
    case object Large extends PublicShipSize

    override def values: IndexedSeq[ShipSize] = findValues
  }

  case object Wharf extends ShipSize

  override def values: IndexedSeq[ShipSize] = findValues ++ PublicShipSize.values
}
