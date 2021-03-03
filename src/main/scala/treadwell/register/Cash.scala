package treadwell.register

import scala.collection.immutable.{SortedMap, SortedSet, TreeMap}

object Cash {
  private val Empty: Cash = Cash(TreeMap.empty)

  /** @return the empty `Cash` object. */
  def apply(): Cash = Empty

  private def apply(denomToCount: SortedMap[Int, Int]): Cash =
    CashVal(denomToCount)

  type Pair = (Int, Int)

  implicit class PairOps(denomWithCount: Pair) {
    def denom: Int = denomWithCount._1
    def count: Int = denomWithCount._2
    def amount: Int = denom * count
  }

  private case class CashVal(denomToCount: SortedMap[Int, Int]) extends Cash
}

/** An immutable, unordered, linear combination of arbitrarily-many positive
  * integers, or ''denominations''. The combination itself may have negative
  * coefficients.
  *
  * Two `Cash` objects with the same `total` value are distinct unless they
  * represent exactly the same combination.
  */
sealed trait Cash {
  import treadwell.register.Cash._

  protected def denomToCount: SortedMap[Int, Int]

  /** @return the count of the given denomination within this combination. */
  def apply(denom: Int): Int = denomToCount.getOrElse(denom, 0)

  /** @return all distinct denominations present within this combination. */
  def denoms: SortedSet[Int] = denomToCount.keySet

  /** @return the total combined value. */
  lazy val total: Int = denomToCount.view.map(_.amount).sum

  /** @return this combination plus a given count of some denomination. */
  def +(p: Pair): Cash = {
    if (0 == p.count) {
      this
    } else {
      val update = p.denom -> (this(p.denom) + p.count)
      Cash(if (0 == update.count) denomToCount-p.denom else denomToCount+update)
    }
  }

  /** @return this combination plus a given vector of denomination counts. */
  def +(ps: Iterable[Pair]): Cash = ps.foldLeft(this) {_ + _}

  /** @return this merged with the given combination. */
  def +(c: Cash): Cash = this + c.denomToCount

  /** @return this less a given vector of denomination counts. */
  def -(ps: Iterable[Pair]): Cash = this + ps.view.map { case (d, c) => d -> -c }

  /** @return this less the given combination. */
  def -(c: Cash): Cash = this - c.denomToCount

  /** @return this combination multiplied by the given scalar value. */
  def *(k: Int): Cash = k match {
    case 0 => Cash()
    case 1 => this
    case _ => Cash(denomToCount.map { p => p.denom -> k*p.count })
  }

  /** Finds a strictly-nonnegative subgroup within this combination that sums
    * to `amount`, and which has the smallest size as well as the largest
    * denominations possible.
    *
    * @return the described sub-combination, if present, otherwise the empty
    *         `Cash` object.
    */
  def draw(amount: Int): Cash = denoms.lastOption
    .flatMap(draw(amount, total, _))
    .getOrElse(Cash())

  private def draw(remaining: Int, // the combined amount left to draw
                   available: Int, // the combined amount left in this object
                       denom: Int): Option[Cash] = denoms.maxBefore(denom) match {
    case None =>
      val count = (remaining/denom) min this(denom)
      if (count*denom == remaining) Some(Cash() + (denom->count)) else None

    case Some(nextDenom) =>
      val max = (remaining/denom) min this(denom)
      val min = 0 max (this(denom) - (available-remaining)/denom)

      (max to min by -1).view.flatMap { count =>
        draw(remaining - count*denom, available - count*denom, nextDenom)
          .map(_ + (denom->count)) // if found, returns the drawn combination
      }.headOption
  }

  /** For testing/debugging purposes. */
  override lazy val toString: String = denomToCount.view
    .map { p => s"$$${p.denom}x${p.count}" }
    .mkString(s"$$$total = (", " + ", ")")
}