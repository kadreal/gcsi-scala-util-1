package com.gaiam.gcsis.util

import org.joda.time.{ReadablePeriod, Period, DateTimeZone, DateTime}
import scalaz.State

/**
 * Created by tstevens on 8/5/14.
 */

protected case class Clock(nextTime : () => DateTime) {

  val now: State[Clock, DateTime] = {
    val date = nextTime()
    State(_ =>  (Clock(nextTime), date) )
  }

  def forward(p : ReadablePeriod) : Clock = {
    Clock(() => nextTime().plus(p))
  }

  def back(p: ReadablePeriod) : Clock = {
    Clock(() => nextTime().minus(p))
  }

}

object Clocks {

  /** Alias for unit */
  def now = unit

  def unit: State[Clock, DateTime] = State(clock => (clock, clock.nextTime()))

  def fixed(fixed: DateTime) = Clock(() => fixed)

  def realTime = Clock(() => DateTime.now)

}
