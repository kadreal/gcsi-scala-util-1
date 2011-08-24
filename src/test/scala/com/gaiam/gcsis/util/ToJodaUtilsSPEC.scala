package com.gaiam.gcsis.util

import org.joda.time.{DateTime, Period, Interval}
import org.specs2.mutable.Specification

/**
 * Created by tstevens on 10/9/14.
 */
object ToJodaUtilsSpec extends Specification {

  val date = new DateTime

  "JODA Utils" should {

    "combine 0 intervals properly" in {

      JodaUtils.combineIntervals(List.empty) mustEqual List.empty

    }
//
//    "combine 1 interval properly" in {
//      val one = List(new Interval(date, Period.days(3)))
//      JodaUtils.combineIntervals(one) mustEqual one
//    }
//
//    "combine 2 overlapping intervals properly" in {
//      val i1 = new Interval(date, Period.days(7))
//      val i2 = new Interval(date.plus(Period.days(3)), Period.days(7))
//
//      JodaUtils.combineIntervals(List(i1, i2)) mustEqual List(new Interval(date, Period.days(14)))
//
//    }
//
//    "combine 2 non overlapping intervals properly" in {
//      val i1 = new Interval(date, Period.days(7))
//      val i2 = new Interval(date.plus(Period.days(8)), Period.days(7))
//      val intervals = List(i1, i2)
//
//      JodaUtils.combineIntervals(intervals) mustEqual intervals
//
//    }
//
//    "combine 3 overlapping intervals properly" in {
//      val i1 = new Interval(date, Period.days(7))
//      val i2 = new Interval(date.plus(Period.days(8)), Period.days(7))
//      val i3 = new Interval(date.plus(3), Period.days(3))
//
//      JodaUtils.combineIntervals(List(i1, i2, i3)) mustEqual List(new Interval(date, Period.days(17)))
//
//    }
//
//    "combine a lot of intervals correctly" in {
//      val i1 = new Interval(date, Period.days(7))
//      val i2 = new Interval(date.plus(Period.days(8)), Period.days(7))
//      val i3 = new Interval(date.plus(3), Period.days(3))
//
//      val futureDate = date.plusYears(1)
//      val i4 = new Interval(futureDate, Period.days(7))
//      val i5 = new Interval(futureDate.plus(Period.days(8)), Period.days(7))
//      val i6 = new Interval(futureDate.plus(3), Period.days(3))
//
//      val intervals = List(i5, i2, i1, i4, i3, i6)
//
//      JodaUtils.combineIntervals(intervals) mustEqual List(new Interval(futureDate, Period.days(17)), new Interval(date, Period.days(17)))
//
//
//    }
//
  }

}
