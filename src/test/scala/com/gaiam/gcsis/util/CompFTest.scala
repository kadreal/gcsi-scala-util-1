package com.gaiam.gcsis.util

import org.junit.Test
import org.junit.Assert._


/**
 * User: tstevens
 * Date: 11/1/12
 */
class CompFTest {

  import CompF._

  @Test
  def testComparePositive {
    assertTrue( comparePositive[Int](None, None) == 0)
    assertTrue( comparePositive(Some(0), None) < 0 )
    assertTrue( comparePositive(None, Some(0)) > 0)
    assertTrue( comparePositive(Some(1), Some(2)) < 0)
    assertTrue( comparePositive(Some(2), Some(1)) > 0)
    assertTrue( comparePositive(Some(1), Some(1)) == 0)
  }

  @Test
  def testOptionalOrdering2 {
    val c = new OptionalOrdering2[Int]()
    assertEquals(-1, c.compare(Some(1), Some(2)))
  }

}
