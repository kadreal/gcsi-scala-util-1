package com.gaiam.gcsis.util

import org.joda.time.DateTime
import org.junit.Test
import org.junit.Assert._

class IntervalTest {
    implicit def dateTime2Ordered(t: DateTime): Ordered[DateTime] = new Ordered[DateTime] {
        override def compare(t2: DateTime) = t.compareTo(t2)
    }

    @Test
    def testIntWithStart = testWithStart(1, 2, 3)

    @Test
    def testIntStartsBefore = testStartsBefore(1, 2)

    @Test
    def testIntIntersect = testIntersect(1, 2, 3, 4)

    @Test
    def testIntUnion = testUnion(1, 2, 3, 4)

    val firstDate = new DateTime("2010-01-01");
    val secondDate = new DateTime("2010-02-01");
    val thirdDate = new DateTime("2010-03-01");
    val fourthDate = new DateTime("2010-04-01");

    @Test
    def testDateTimeWithStart = testWithStart(firstDate, secondDate, thirdDate)

    @Test
    def testDateTimeStartsBefore = testStartsBefore(firstDate, secondDate)

    @Test
    def testDateTimeIntersect = testIntersect(firstDate, secondDate, thirdDate, fourthDate)

    @Test
    def testDateTimeUnion = testUnion(firstDate, secondDate, thirdDate, fourthDate)

    // You should be able to factor out duplicated code to make the tests simpler!

    def testWithStart[T <% Ordered[T]](first: T, second: T, third: T) = {
        // create intervals over integer ranges and call interval.withStart to create
        // a new immutable interval.

        // Check that the end dates of the newly created interval are unchanged but that the
        // start date of the newly created interval is as specified.
        val complete: Interval[T] = Interval(Some(second), Some(third)).withStart(Some(first))
        assertTrue(complete.start.exists(_ == first))
        assertTrue(complete.end.exists(_ == third))

        val noStart: Interval[T] = Interval(Some(second), Some(third)).withStart(None)
        assertTrue(noStart.start.isEmpty)
        assertTrue(noStart.end.exists(_ == third))

        val noEnd: Interval[T] = Interval(Some(second), None).withStart(Some(first))
        assertTrue(noEnd.start.exists(_ == first))
        assertTrue(noEnd.end.isEmpty)
    }

    def testStartsBefore[T <% Ordered[T]](first: T, second: T) = {
        // create intervals over integer ranges and call interval.startsBefore
        // to test the ordering of the intervals
        assertTrue(Interval(Some(first), Some(second)).startsBefore(Interval(Some(second),None)))
        assertTrue(Interval(None, Some(second)).startsBefore(Interval(Some(second),None)))
        assertFalse(Interval(None, Some(second)).startsBefore(Interval(None,None)))
        assertTrue(Interval(Some(first), None).startsBefore(Interval(Some(second),None)))
    }

    def testIntersect[T <% Ordered[T]](first: T, second: T, third: T, fourth: T) = {
        // create intervals over integer ranges and call interval.intersect to (potentially) create
        // a new immutable interval.

        // Check that when an intersection exists, the start of the resultant interval
        // is equal to the maximum start value and that the end of the interval
        // is equal to the minimum end value

        val complete: Option[Interval[T]] = Interval(Some(first), Some(third)).intersect(Interval(Some(second), Some(fourth)))
        assertTrue(complete.exists(_ == Interval(Some(second), Some(third))))

        val noStart: Option[Interval[T]] = Interval(None, Some(third)).intersect(Interval(Some(second), Some(fourth)))
        assertTrue(noStart.exists(_ == Interval(Some(second), Some(third))))

        val noEnd: Option[Interval[T]] = Interval(Some(first), None).intersect(Interval(Some(second), Some(fourth)))
        assertTrue(noEnd.exists(_ == Interval(Some(second), Some(fourth))))

        val infinite: Option[Interval[T]] = Interval[T](None, None).intersect(Interval[T](None, None))
        assertTrue(infinite.exists(_ == Interval[T](None, None)))

        val infStart: Option[Interval[T]] = Interval(None, Some(first)).intersect(Interval(None, Some(second)))
        assertTrue(infStart.exists(_ == Interval(None, Some(first))))

        val infEnd: Option[Interval[T]] = Interval(Some(first), None).intersect(Interval(Some(second), None))
        assertTrue(infEnd.exists(_ == Interval(Some(second), None)))

        // Check the condition where no intersection exists
        val disjoint: Option[Interval[T]] = Interval(Some(first), Some(second)).intersect(Interval(Some(third), Some(fourth)))
        assertTrue(disjoint.isEmpty)

        val disrev: Option[Interval[T]] = Interval(Some(third), Some(fourth)).intersect(Interval(Some(first), Some(second)))
        assertTrue(disrev.isEmpty)
    }

    def testUnion[T <% Ordered[T]](first: T, second: T, third: T, fourth: T) = {
        // create intervals over integer ranges and call interval.union. If the intervals
        // do not overlap, a tuple of the two original intervals should be returned;
        // otherwise, merge the two into a single interval.

        // Check that when an intersection exists, the start of the resultant interval
        // is equal to the minimum start value and that the end of the interval
        // is equal to the maximum end value

        val complete = Interval(Some(first), Some(third)).union(Interval(Some(second), Some(fourth)))
        assertTrue(complete.left.exists(_ == Interval(Some(first), Some(fourth))))

        val noStart = Interval(None, Some(third)).union(Interval(Some(second), Some(fourth)))
        assertTrue(noStart.left.exists(_ == Interval(None, Some(fourth))))

        val noEnd = Interval(Some(first), None).union(Interval(Some(second), Some(fourth)))
        assertTrue(noEnd.left.exists(_ == Interval(Some(first), None)))

        val infinite = Interval[T](None, None).union(Interval[T](None, None))
        assertTrue(infinite.left.exists(_ == Interval[T](None, None)))

        val infStart = Interval(None, Some(first)).union(Interval(None, Some(second)))
        assertTrue(infStart.left.exists(_ == Interval(None, Some(second))))

        val infEnd = Interval(Some(first), None).union(Interval(Some(second), None))
        assertTrue(infEnd.left.exists(_ == Interval(Some(first), None)))

        // Check the condition where no intersection exists
        val disjoint = Interval(Some(first), Some(second)).union(Interval(Some(third), Some(fourth)))
        assertTrue(disjoint.right.exists(_ == (Interval(Some(first), Some(second)), Interval(Some(third), Some(fourth)))))

        val disjointInfStart = Interval(None, Some(second)).union(Interval(Some(third), Some(fourth)))
        assertTrue(disjointInfStart.right.exists(_ == (Interval(None, Some(second)), Interval(Some(third), Some(fourth)))))

        val disjointInfEnd = Interval(Some(first), Some(second)).union(Interval(Some(fourth), None))
        assertTrue(disjointInfEnd.right.exists(_ == (Interval(Some(first), Some(second)), Interval(Some(fourth), None))))
    }

}
