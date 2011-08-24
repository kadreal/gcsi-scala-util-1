/*
 * Schedule.scala
 * Copyright, Gaiam Inc.
 */

package com.gaiam.gcsis.util

import org.joda.time.{Interval => JInterval}
import org.joda.time.Instant
import org.joda.time.ReadableInstant
import scala.util.Sorting

class IntervalSchedule(seq: Seq[JInterval]) {
    val intervals: Array[JInterval] = Sorting.stableSort(seq, (a: JInterval, b: JInterval) => a.getStart.compareTo(b.getStart) <= 0)

    private def combine(i1: JInterval, i2: JInterval) = {
        if (i1.contains(i2)) i1
        else if (i2.contains(i1)) i2
        else new JInterval(i1.getStart, i2.getEnd)
    }

    def continuousInterval(from: ReadableInstant) = {
        intervals.foldLeft[Option[JInterval]](None)(
            (result, interval) => {
             (result, interval) match {
                case (None, i: JInterval) if i.contains(from) => Some(i)
                case (None, _) => None
                case (Some(res: JInterval), i: JInterval) if res.overlaps(i) || res.abuts(i) => Some(combine(res, i))
                case _ => result
             }
            })
    }
}
