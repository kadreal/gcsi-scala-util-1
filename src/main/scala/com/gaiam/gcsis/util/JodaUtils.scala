package com.gaiam
package gcsis.util

import java.math.BigDecimal
import org.joda.time.{Interval, Period, ReadableInstant, DateTime}

object JodaUtils {

  trait Implicits {

    implicit def instantOrdered[T <: ReadableInstant](d: T): Ordered[T] = new Ordered[T] {
      def compare(o: T) = d.compareTo(o)
    }

    implicit def dateTimeOrdering = new scala.math.Ordering[DateTime] {
      def compare(x: DateTime, y: DateTime) = x.compareTo(y)
    }

    implicit def instantOrdering[T <: ReadableInstant]: scala.math.Ordering[T] = new scala.math.Ordering[T] {
      override def compare(o1: T, o2: T) = o1.compareTo(o2)
    }

    implicit def RichJodaInterval(ival: org.joda.time.Interval): RichJodaInterval = new RichJodaInterval(ival)

    implicit def periodOrdering: scala.math.Ordering[Period] = new scala.math.Ordering[Period] {
      override def compare(o1: Period, o2: Period) = o1.toStandardDuration.compareTo(o2.toStandardDuration)
    }
  }

  object implicits extends Implicits

  class RichJodaInterval(ival: org.joda.time.Interval) {
    def fractionBefore(date: DateTime): BigDecimal = {
      if (date.isBefore(ival.getStart)) {
        BigDecimal.ZERO
      } else {
        val endDuration = new BigDecimal(ival.withEnd(date).toDurationMillis)
        val totalDuration = new BigDecimal(ival.toDurationMillis)
        endDuration.divide(totalDuration, java.math.MathContext.DECIMAL64)
      }
    }
  }

  object JodaIndefiniteInterval {

    implicit object DateTimeBehavior extends IndefiniteIntervalBehavior[DateTime, IndefiniteInterval] {

      /** Create a new type of IndefiniteInterval */
      def create(s: DateTime, e: Option[DateTime]) = new IndefiniteInterval[DateTime]() {
        def start = s
        def end = e
      }
    }
  }

  def toIndefiniteInterval(interval: org.joda.time.Interval): IndefiniteInterval[DateTime] = new IndefiniteInterval[DateTime] {
    def start = interval.getStart

    def end = Some(interval.getEnd)
  }

  implicit def toJodaUtils(i: IndefiniteInterval[DateTime]): ToJodaUtils = new ToJodaUtils {
    val interval = i
  }

  trait ToJodaUtils {
    val interval: IndefiniteInterval[DateTime]
    def toJodaPeriod = interval.end.map(end => new Period(interval.start, end))
    def toJodaInterval = interval.end.map(new org.joda.time.Interval(interval.start, _))
    def toJodaInterval(defaultEnd: => DateTime) = new org.joda.time.Interval(interval.start, interval.end.getOrElse(defaultEnd))


    def fractionBefore(dateTime: DateTime): Option[BigDecimal] = {
      interval.end.map(end => new BigDecimal((dateTime.getMillis - interval.start.getMillis) / (end.getMillis - interval.start.getMillis)))
    }

  }

  /**
   * Combine overlapping intervals together.  This is a little tricky because a combining an interval means the
   * newly created interval may overlap an interval that previously didn't overlap anything.
   *
   */
  def combineIntervals(toCombine: List[Interval]): List[Interval] = {

    import implicits._

    def allThatOverlap(interval: Interval, intervals: List[Interval]) = intervals.partition(_.overlaps(interval))

    /** List of intervals with whether or not that interval has been checked. */
    def combine(intervals: Map[Interval, Boolean]): Iterable[Interval] = {

      intervals.filter( ib => !ib._2).keys.toList.sortBy(_.getStart) match {
        case Nil => intervals.keys //all have been tested, so we can stop and use the current list
        case head :: tail => {
          val (yes, no) = allThatOverlap(head, intervals.keys.filter(_ != head).toList)
          yes match {
            case Nil => {
              combine(intervals.updated(head, true))
            }
            case overlaps => {
              val newInterval = overlaps.foldLeft(head)((h, i) => {
                val start = if (h.getStart <= i.getStart) h.getStart else i.getStart
                new Interval(start, h.toPeriod.plus(i.toPeriod))
              })
              //must start over combining
              val newIntervals = newInterval :: no
              combine(Map(newIntervals.zip(Stream.continually(false)) :_*))
            }
          }
        }
      }
    }

    combine(Map(toCombine.zip(Stream.continually(false)) :_* )).toList

  }


}