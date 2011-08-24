package com.gaiam.gcsis.util

import java.math.BigDecimal
import scalaz.{Equal, Monoid}

/**
 * Created by tstevens on 5/22/14.
 */
object RichBigDecimal {

  implicit def bigDecimalOrdered(a: BigDecimal) = new Ordered[BigDecimal] {
    override def compare(that: BigDecimal): Int = a.compareTo(that)
  }

  implicit object BigDecimalNumeric extends scala.math.Numeric[BigDecimal] with Ordering[BigDecimal] with Monoid[BigDecimal] with Equal[BigDecimal] {
    override def compare(o1: BigDecimal, o2: BigDecimal) = o1.compareTo(o2)
    override def plus(x: BigDecimal, y: BigDecimal) = x.add(y)
    override def minus(x: BigDecimal, y: BigDecimal) = x.subtract(y)
    override def times(x: BigDecimal, y: BigDecimal) = x.multiply(y)
    override def negate(x: BigDecimal) = x.negate
    override def fromInt(x: Int) = new BigDecimal(x)
    override def toInt(x: BigDecimal) = x.intValue
    override def toLong(x: BigDecimal) = x.longValue
    override def toFloat(x: BigDecimal) = x.floatValue
    override def toDouble(x: BigDecimal) = x.doubleValue
    override val one = BigDecimal.ONE
    override val zero = BigDecimal.ZERO
    override def append(x: BigDecimal, y: => BigDecimal) = x.add(y)
    override def equal(a: BigDecimal, b: BigDecimal) = a.compareTo(b) == 0
  }

}
