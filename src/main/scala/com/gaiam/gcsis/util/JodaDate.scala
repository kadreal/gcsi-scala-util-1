package com.gaiam.gcsis.util

import org.joda.time.DateTime

/**
 * User: travis.stevens@gaiam.com
 * Date: 1/24/13
 */
object JodaDate {
  implicit def dayOrdering = new Ordering[DateTime] {
    def compare(x: DateTime, y: DateTime) =
      if (x.getYear < y.getYear || x.getYear == y.getYear && x.getDayOfYear < x.getDayOfYear) -1
      else if (x.getYear == y.getYear && x.getDayOfYear == y.getDayOfYear) 0
      else -1
  }
}
