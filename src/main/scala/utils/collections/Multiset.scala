package utils.collections

class Multiset[A](private val map: Map[A, Int]):

  import Multiset.*

  def toMap = map

  def toSet = map.keySet

  def +(elem: A) = new Multiset(innerAdd(map, elem, 1))

  def except(elem: A) = new Multiset(map.removed(elem))

  def except(keys: IterableOnce[A]) = new Multiset(map.removedAll(keys))

  def ++(ms: Multiset[A]) = new Multiset(ms.map.foldLeft(map) { case (m, (e, c)) => innerAdd(m, e, c) })

  def count(elem: A): Int = map.getOrElse(elem, 0)

  def contains(elem: A): Boolean = count(elem) > 0

object Multiset:

  def apply[A](elems: A*): Multiset[A] = new Multiset(elems.groupMapReduce(e => e)(e => 1)(_ + _))

  private def innerAdd[A](map: Map[A, Int], elem: A, count: Int): Map[A, Int] = map.updatedWith(elem) {
    case None => 
      if count > 0 then
        Some(count) 
      else
        None
    case Some(c) => 
      val newTotal = c + count
      if newTotal > 0 then
        Some(newTotal)
      else
        None
  }