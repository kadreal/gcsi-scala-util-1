package com.gaiam.gcsis.util

import scalaz._
import java.math.BigDecimal

/**
 * Created by tstevens on 11/4/14.
 */
trait BigDecimalInstances {
  implicit def BigDecimalInstance: Monoid[BigDecimal] with Show[BigDecimal] with Order[BigDecimal] with Semigroup[BigDecimal] = new Monoid[BigDecimal] with Show[BigDecimal] with Order[BigDecimal]{
    override def shows(f: BigDecimal) = f.toString

    def append(f1: BigDecimal, f2: => BigDecimal) = f1 add f2

    def zero = BigDecimal.ZERO

    def order(x: BigDecimal, y: BigDecimal) = x.compareTo(y) match {
      case x if x < 0   => Ordering.LT
      case x if x == 0 => Ordering.EQ
      case x if x > 0   => Ordering.GT
    }
  }

  import Tags.Multiplication

//  implicit def BigDecimalMultiplication: Monoid[BigDecimal @@ Multiplication] with Order[BigDecimal @@ Multiplication] with Show[BigDecimal @@ Multiplication] = new Monoid[BigDecimal @@ Multiplication] with Order[BigDecimal @@ Multiplication] with Show[BigDecimal @@ Multiplication] {
//    override def shows(f: scalaz.@@[BigDecimal, Multiplication]) = f.toString
//
//    def append(f1: BigDecimal @@ Multiplication, f2: => BigDecimal @@ Multiplication) = Multiplication(f1 multiply f2)
//
//    def zero: BigDecimal @@ Multiplication = Multiplication(BigDecimal.ONE)
//
//    def order(x: BigDecimal @@ Multiplication, y: BigDecimal @@ Multiplication) = x.compareTo(y) match {
//      case x if x < 0   => Ordering.LT
//      case x if x == 0 => Ordering.EQ
//      case x if x > 0   => Ordering.GT
//    }
//  }
}
