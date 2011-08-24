package com.gaiam.gcsis.util

import scalaz.{Ordering, Order}

/**
 * Created by tstevens on 11/4/14.
 */
trait DefaultPackage extends

  //Allows |> (thrush) <| .left (to -\/) .right (to \/-)
  scalaz.syntax.ToIdOps with

  //Allows wrapNel
  scalaz.syntax.ToNelOps with

  //allows \/> and <\/, etc
  scalaz.syntax.ToOptionalOps with

  scalaz.syntax.std.ToOptionOps with

  scalaz.syntax.ToEitherOps with

  //Allows ===
  scalaz.syntax.ToEqualOps with

  // > = <
  scalaz.syntax.ToOrderOps with

  //allows traverse and sequence functions
  scalaz.syntax.ToTraverseOps with

  scalaz.syntax.ToShowOps with

  //allows |+|
  scalaz.syntax.ToSemigroupOps with

  //allows suml
  scalaz.syntax.ToMonoidOps with

  scalaz.syntax.ToApplicativeOps with

  //type-class instances of the above operations
  scalaz.std.AnyValInstances with
  scalaz.std.StringInstances with
  scalaz.std.OptionInstances with
  scalaz.std.OptionFunctions with
  scalaz.std.EitherInstances with
  scalaz.std.ListInstances with
  scalaz.std.IntFunctions with
  scalaz.std.LongFunctions with
  scalaz.std.TupleInstances with
  BigDecimalInstances with
  UuidInstances with
  scalaz.contrib.nscala_time.Instances with
  JavaLangInstances
  {



}

/** Typeclass instances for java.lang classes that are not provided by scalaz (or maybe just not found)*/
trait JavaLangInstances {
  import java.lang.{Long => JLong}
  implicit object longEquals extends Order[JLong] {

    override def order(x: JLong, y: JLong): Ordering =
      if (x == y) Ordering.EQ
      else if (x < y) Ordering.LT
      else Ordering.GT
  }

  import java.lang.{Integer => JInt}
  implicit object intEquals extends Order[JInt] {


    override def order(x: JInt, y: JInt): Ordering =
      if (x == y) Ordering.EQ
      else if (x < y) Ordering.LT
      else Ordering.GT
  }

  import java.lang.{Float => JFloat}
  implicit object floatEquals extends Order[JFloat] {

    override def order(x: JFloat, y: JFloat): Ordering =
      if (x == y) Ordering.EQ
      else if (x < y) Ordering.LT
      else Ordering.GT
  }

}





