package com.gaiam.gcsi.util

import org.joda.time.DateTime
import org.joda.time.ReadablePeriod

/**
 * This interface provides an abstraction of finding the current time. It is
 * useful in testing to be able to have all the clocks in a system return
 * a predictable timestamp.
 *
 * @see Clocks
 * @author knuttycombe
 */
trait Clock {
  /**
   * Return the current time according to this clock.
   * @return
   */
  def now: DateTime
}

/**
 * A utility class providing factories for some standard Clock implementation.
 * @author knuttycombe
 */
object Clocks {
  /**
   * A broken clock is right once a day... this is a broken clock that's simply right once.
   *
   * @param date
   * @return
   */
  def fixed(on: DateTime): Clock = {
    new Clock {
      def now: DateTime = {
        on
      }
    }
  }

  val realtime = new Clock {
    def now: DateTime = {
      new DateTime
    }
  }
}

case class ManualClock(now: DateTime) extends Clock {

  def forward(period: ReadablePeriod): ManualClock = {
    this.copy(now = now.plus(period))
  }

}