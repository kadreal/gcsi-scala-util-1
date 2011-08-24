package com.gaiam.gcsis.util

import org.joda.time.DateTime
import org.specs2.mutable.SpecificationWithJUnit
import JodaUtils.implicits._

import scalaz.\/-


/**
 * Created by tstevens on 10/26/14.
 */
class ClockSpec extends SpecificationWithJUnit {

  "clocks" should {

    "fixed clock returns fixed time" in {
      val time = new DateTime(2014,1,1,0,0)
      val fixed = Clocks.fixed(time)

      val now = fixed.now

      val x = now.apply(fixed)

      x._2 mustEqual time


    }

    "realtime clock returns same value even if callsed 50ms later" in {
      val clock = Clocks.realTime

      val first = clock.now

      Thread.sleep(50)

      val second = clock.now

      first(clock)._2 mustEqual second(clock)._2

    }

    "realtime clock, successive state should return successive time" in {
      val clock = Clocks.realTime

      val first = Clocks.now

      Thread.sleep(50)

      val second = first(clock)._1.now

      first(clock)._2 must beLessThan(second(clock)._2)

    }

//    "combine state and either" in {
//
//      def nameV = \/-("Joe")
//      val time = new DateTime
//
//      val x = for {
//        now <- Clocks.now
//        name <- nameV
//      } yield { (now, name)}
//
//      x(Clocks.fixed(time)) mustEqual (time, "Jod")
//
//    }



  }

}
