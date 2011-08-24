/*
 * FJ.scala
 */

package com.gaiam
package gcsis.util

import fj.data.Option.{some, none}
import fj.data.Either.{left, right}
import fj.F
import java.math.BigDecimal
import scalaz.Equal
import scalaz.Monoid

object FJ {
    trait Implicits {
        implicit def RichScalaF[A,B](f: A => B): RichScalaF[A,B] = new RichScalaF(f)

        implicit def RichFJOption[T](o: fj.data.Option[T]): RichFJOption[T] = new RichFJOption(o)

        implicit def RichScalaOption[T](o: scala.Option[T]): RichScalaOption[T] = new RichScalaOption(o)

        implicit def RichFJEither[T,U](e: fj.data.Either[T,U]): RichFJEither[T,U] = new RichFJEither(e)

        implicit def RichScalaEither[T,U](o: scala.Either[T,U]): RichScalaEither[T,U] = new RichScalaEither(o)

        implicit def ord[T <: Comparable[T]](x: T): Ordered[T] = new Ordered[T] {
            def compare(o: T) = x.compareTo(o)
        }

        implicit def ordering[T <: Comparable[T]]: Ordering[T] = new Ordering[T] {
            override def compare(o1: T, o2: T) = o1.compareTo(o2)
        }

        implicit def bigDecimalOrdered(a: BigDecimal) = ord(a)

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

    object implicits extends Implicits

    class RichScalaF[A,B](ff: A => B) {
        def asJava: F[A,B] = new F[A,B] {
            override def f(a: A) = ff(a)
        }
    }

    class RichFJOption[T](val o: fj.data.Option[T]) {
        def asScala: scala.Option[T] = {
            if (o.isSome) scala.Some(o.some)
            else scala.None
        }
    }

    class RichScalaOption[T](val o: scala.Option[T]) {
        def asJava[U >: T]: fj.data.Option[U] = o match{
            case scala.Some(a) => some(a)
            case scala.None => none[U]
        }
    }

    class RichFJEither[T,U](val e: fj.data.Either[T,U]) {
        def asScala: scala.Either[T,U] = {
            if (e.isLeft) scala.Left(e.left.value)
            else scala.Right(e.right.value)
        }
    }

    class RichScalaEither[T,U](val o: scala.Either[T,U]) {
        def asJava[X >: T, Y >: U]: fj.data.Either[X, Y] = o match {
            case scala.Left(a) => left(a)
            case scala.Right(b) => right(b)
        }
    }
}
