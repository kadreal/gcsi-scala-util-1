package com.gaiam.gcsi.util

/**
 *
 * @author knuttycombe
 */
trait Sequence {
  def nextValue: Long
}

case class MemorySequence(var base: Long = 0L) extends Sequence {

  def nextValue: Long = {
      base += 1; base
  }

}