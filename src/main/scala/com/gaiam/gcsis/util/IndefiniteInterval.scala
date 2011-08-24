package com.gaiam.gcsis.util

import scala.math.Ordered
import scala.math.Ordering

/**
 * Indefinite intervals have a defined start point and a potentially defined end point.
 * The interval is always inclusive of the start and exclusive of the end if it is defined;
 * if the end is undefined then the interval is considered to be inclusive of all values greater
 * than the start.
 *
 * T is the object providing the endpoints.
 * U is the implementation of this trait.
 *
 * @author knuttycombe
 * @author tstevens - ported to scala
 */


/** the basic data type */
trait IndefiniteInterval[T] {
  def start: T
  def end: Option[T]
  def isInfinite = end.isEmpty

}

/** TypeClass defining the behavior on the indefinite interval. */
trait IndefiniteIntervalBehavior[T, I[T] <: IndefiniteInterval[T]] {

  /** Use the newStart to define the new start of the interval. */
  def withStart(interval: I[T], newStart: T) = create(newStart, interval.end)
  /** Use the newEnd to define the new end of the interval or none if it doesn't end. */
  def withEnd(interval: I[T], newEnd: Option[T]) = create(interval.start, newEnd)
  /** Create a new type of IndefiniteInterval */
  def create(start: T, end: Option[T]) : I[T]
}

case class IndefiniteIntervalT[T](start: T, end: Option[T]) extends IndefiniteInterval[T]

object IndefiniteIntervalT {
  implicit def indefiniteIntervalTBehavior[T] = IndefiniteIntervalTBehavior[T]

  case class IndefiniteIntervalTBehavior[T]() extends IndefiniteIntervalBehavior[T, IndefiniteIntervalT] {
    /** Create a new type of IndefiniteInterval */
    def create(start: T, end: Option[T]) = IndefiniteIntervalT(start,end)
  }
}

object DefaultIndefiniteInterval {

  implicit def toDefaultBehavior[T](interval: IndefiniteInterval[T]) = new IndefiniteIntervalBehavior[T, IndefiniteInterval] {
    def create(newStart: T, newEnd: Option[T]) = new IndefiniteInterval[T] {
      def start = newStart
      def end = newEnd
    }
  }
}

object IndefiniteInterval {

  def apply[T](startT: T) = new IndefiniteInterval[T] {
    val start = startT
    val end = None
  }

  def apply[T](startT: T, endT:T) = new IndefiniteInterval[T] {
    val start = startT
    val end = Some(endT)
  }

  def apply[T](startT: T, endT: Option[T]) = new IndefiniteInterval[T] {
    val start = startT
    val end = endT
  }

  def indefiniteIntervalOrdering[T](implicit ord: Ordering[T]): Ordering[IndefiniteInterval[T]] = new Ordering[IndefiniteInterval[T]] {
    def compare(i1: IndefiniteInterval[T], i2: IndefiniteInterval[T]) = IndefiniteInterval.compare(i1, i2)(ord)
  }

  implicit def addBehavior[T, I[T] <: IndefiniteInterval[T]](i: I[T])(implicit ibehavior: IndefiniteIntervalBehavior[T,I]): IntervalWithBehavior[T,I] =
    new IntervalWithBehavior[T,I] {
      val interval = i
      val behavior = ibehavior
    }

  implicit def toIntervalBehaviorOperations[T,I[T] <: IndefiniteInterval[T]](i: I[T])(implicit ibehavior: IndefiniteIntervalBehavior[T,I], iordering: Ordering[T]): IndefiniteIntervalBehaviorOperations[T, I] =
    new IndefiniteIntervalBehaviorOperations[T, I] {
      val interval = i
      val ordering = iordering
      val behavior = ibehavior
    }

  implicit def toIndefiniteIntervalOrderingOptions[T](i: IndefiniteInterval[T])(implicit iordering: Ordering[T]):IndefiniteIntervalOrderingOperations[T] = {
    new IndefiniteIntervalOrderingOperations[T] {
      val interval = i
      val ordering = iordering
    }
  }

  /** Trait part of pimp my library pattern, pimps IndefiniteInterval with intersect and union. */
  trait IndefiniteIntervalBehaviorOperations[T, I[T] <: IndefiniteInterval[T]] {
    val interval: I[T]
    val ordering: Ordering[T]
    val behavior: IndefiniteIntervalBehavior[T,I]
    def intersect(i2: I[T]) = IndefiniteInterval.intersect(interval, i2)(ordering, behavior)
    def union(i2: I[T]) = IndefiniteInterval.union(interval, i2)(ordering, behavior)
  }

  /** Trait part of pimp my library pattern, pimps IndefiniteInterval with contains and compare.  Does not require IndefiniteIntervalBehaviorPattern */
  trait IndefiniteIntervalOrderingOperations[T] extends Ordered[IndefiniteInterval[T]]{
    val interval: IndefiniteInterval[T]
    val ordering: Ordering[T]
    def contains(t: T) = IndefiniteInterval.contains(interval, t)(ordering)
    def compare(that: IndefiniteInterval[T]) = IndefiniteInterval.compare(interval, that)(ordering)
  }

  trait IntervalWithBehavior[T, I[T] <: IndefiniteInterval[T]] {
    val interval: I[T]
    val behavior: IndefiniteIntervalBehavior[T,I]
    def withStart(start: T) = behavior.withStart(interval, start)
    def withEnd(end: Option[T]) = behavior.withEnd(interval, end)
    def create(start: T, end: Option[T]) = behavior.create(start,end)
  }

  def compare[T](i1: IndefiniteInterval[T], i2: IndefiniteInterval[T])(implicit ord: Ordering[T]): Int =
    ord.compare(i1.start, i2.start) | CompF.comparePositive(i1.end, i2.end)

  def intersect[T,I[T] <: IndefiniteInterval[T]](i1: I[T], i2: I[T])(implicit ord: Ordering[T], behavior: IndefiniteIntervalBehavior[T,I]): Option[I[T]] = {
    import ord._
    if (i2.start >= i1.start) {
      if (isInfinite(i1)) Some(i2)
      else if (i1.end.exists(_ < i2.start)) None
      else {
        implicit val endOrdering = CompF.OptionalOrdering[T]
        import endOrdering._
        val end = if (i1.end < i2.end) i1.end else i2.end
        Some(behavior.create(i2.start, end))
      }
    }
    else intersect[T, I](i2, i1)
  }

  def intersects[T,I[T] <: IndefiniteInterval[T]](i1: I[T], i2: I[T])(implicit ord: Ordering[T], behavior: IndefiniteIntervalBehavior[T,I]): Boolean = {
    intersect(i1, i2).isDefined
  }



  def union[T, I[T] <: IndefiniteInterval[T]](i1: I[T], i2: I[T])(implicit ord: Ordering[T], behavior: IndefiniteIntervalBehavior[T,I]): Either[I[T], (I[T], I[T]) ] = {
    import ord._
    if (i2.start >= i1.start) {
      if (isInfinite(i1)) Left(i1)
      else if (i1.end.exists(_ < i2.start)) Right((i1, i2))
      else {
        implicit val endOrdering = CompF.OptionalOrdering[T]
        import endOrdering._
        val end = if (i1.end < i2.end) i2.end else i1.end
        Left(behavior.withEnd(i1, end))
      }
    }
    else {
      union[T, I](i2, i1)
    }
  }

  def isInfinite[I <: IndefiniteInterval[_]](interval: I): Boolean = {
    interval.end.isEmpty
  }

  /**
   * Tests for inclusion of a value within an interval. The test is inclusive
   * of start date and exclusive of end date.
   */
  def contains[T, I[T] <: IndefiniteInterval[T]](interval: I[T], value: T)(implicit ord: Ordering[T]): Boolean = {
    import ord._
    interval.start <= value && interval.end.forall(_ > value)
  }

  def merge[T,I[T] <: IndefiniteInterval[T]](intervals: List[I[T]])(implicit ord: Ordering[T], behavior: IndefiniteIntervalBehavior[T,I]): List[I[T]] = {

    def mergeSorted[A, J[A] <: IndefiniteInterval[A]](intervals: List[J[A]])(implicit uord: Ordering[A], ubehavior: IndefiniteIntervalBehavior[A,J]): List[J[A]] = {
      if (intervals.size <= 1) intervals
      else {
        val i1 = intervals(0)
        val i2 = intervals(1)
        union[A,J](i1, i2)(uord, ubehavior) match {
          case Left(fullInterval) => mergeSorted[A,J](fullInterval :: intervals.tail.tail)
          case Right( (first, second) ) => first :: mergeSorted[A,J](second :: intervals.tail.tail)
        }
      }
    }



    mergeSorted[T,I](intervals.sorted(indefiniteIntervalOrdering(ord)))
  }



}
