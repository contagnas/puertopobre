package v2

import cats.Monoid
import cats.implicits._

import scala.util.Random

object Count {
  def apply[K](values: (K, Int)*): Count[K] = {
    values.foreach(kv => if (kv._2 < 0) throw noNegativeNumbersException)
    val filtered = values.filter(_._2 > 0)
    new Count(filtered.toMap)
  }

  def empty[K] = new Count(Map.empty[K, Int])

  implicit def CountSemigroup[K]: Monoid[Count[K]] = new Monoid[Count[K]] {
    override def combine(x: Count[K], y: Count[K]): Count[K] =
      new Count(x.map |+| y.map)

    override def empty: Count[K] = Count.empty
  }

  val noNegativeNumbersException = new IllegalArgumentException("No negative numbers")
}

class Count[K] private (private val map: Map[K, Int]) {
  def get(item: K): Int = map.getOrElse(item, 0)

  def total: Int = map.values.sum

  def set(item: K, count: Int): Count[K] =
    if (count < 0) throw Count.noNegativeNumbersException
    else if (count == 0) new Count(map - item)
    else new Count(map.updated(item, count))

  def update(item: K, f: Int => Int): Count[K] =
    set(item, f(get(item)))

  def contains(item: K): Boolean = map.contains(item)

  val exists: K => Boolean = contains

  def totalWhere(p: K => Boolean): Int =
    map.collect { case (k, v) if p(k) => v }.sum

  def nonZeroItems: Set[K] = map.keySet

  def toList: List[(K, Int)] = map.toList

  def takeRandom(rng: Random = new Random): (Count[K], K) = {
    val selectedWeight = rng.between(0, total)

    var countSeen = 0
    for ((item, count) <- map) {
      countSeen += count
      if (countSeen >= selectedWeight)
        return (update(item, _ - 1), item)
    }
    throw new IllegalStateException(s"Found no item in CDF with total $total and selected weight $selectedWeight")
  }

  override def toString: String = {
    val valueStrings = map.map { case (k, v) => s"$k -> $v" }.mkString(", ")
    s"Count($valueStrings)"
  }
}

