package com.gaiam.gcsis.util

/*
 * ScheduleTest.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import org.junit.After
import org.junit.Before
import org.junit.Test
import org.junit.Assert._

import org.joda.time.{Interval => JInterval}
import org.joda.time.Instant
import org.joda.time.DateTime


class ScheduleTest {

    val i1 : JInterval = new JInterval(new DateTime("2010-01-01"), new DateTime("2010-02-01"))
    val i2 : JInterval = new JInterval(new DateTime("2010-02-01"), new DateTime("2010-03-01"))
    val i3 : JInterval = new JInterval(new DateTime("2010-03-01"), new DateTime("2010-04-01"))
    val i4 : JInterval = new JInterval(new DateTime("2010-01-15"), new DateTime("2010-03-15"))

    @Before
    def setUp: Unit = {
    }

    @After
    def tearDown: Unit = {
    }

    @Test
    def testOneIntervalContinuousInterval = {
        val schedule = new IntervalSchedule(Array(i1))
        val begin = new DateTime("2010-01-01")
        val interval = schedule.continuousInterval(begin)
        assertTrue(interval.isDefined)
        assertEquals(begin, interval.get.getStart)
        assertEquals(i1.getEnd, interval.get.getEnd)
    }

    @Test
    def testTwoOverlappingIntervalContinuousInterval = {
        val schedule = new IntervalSchedule(Array(i1, i4))
        val date = new DateTime("2010-01-20")
        val interval = schedule.continuousInterval(date)
        assertTrue(interval.isDefined)
        assertTrue("interval '" + interval + "' doesn't contain date '" + date + "'", interval.get.contains(date))
        assertEquals(i4.getEnd, interval.get.getEnd)

    }

    @Test
    def testAbutsIntervalContinuousInterval = {
        val schedule = new IntervalSchedule(Array(i1, i2, i3))
        val date = new DateTime("2010-01-20")
        val interval = schedule.continuousInterval(date)
        assertTrue(interval.isDefined)
        assertTrue(interval.get.contains(date))
        assertEquals(i3.getEnd, interval.get.getEnd)
    }

}
