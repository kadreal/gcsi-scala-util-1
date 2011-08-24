package com.gaiam
package gcsis.util

import java.math.BigDecimal
import scalaz._
import org.joda.time.{Period, ReadableInstant, DateTime}

object JodaUtils {
    trait Implicits {
        implicit def ord[T <: ReadableInstant](d: T): Ordered[T] = new Ordered[T] {
            def compare(o: T) = d.compareTo(o)
        }

        implicit def ordering[T <: ReadableInstant]: scala.math.Ordering[T] = new scala.math.Ordering[T] {
            override def compare(o1: T, o2: T) = o1.compareTo(o2)
        }

        implicit object equal extends Equal[DateTime] {
            override def equal(a: DateTime, b: DateTime) = a.compareTo(b) == 0
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
                val endDuration = new BigDecimal(ival.withEnd(date).toDurationMillis());
                val totalDuration = new BigDecimal(ival.toDurationMillis());
                endDuration.divide(totalDuration, java.math.MathContext.DECIMAL64);
            }
        }
    }

}