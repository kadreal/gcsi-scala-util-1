package com.gaiam.gcsis.util

import org.junit.Test
import org.junit.Assert._
import com.gaiam.gcsis.util

/**
 * User: tstevens
 * Date: 11/1/12
 */
class IndefiniteIntervalTest {

  import IndefiniteInterval._

  implicit val intBehavior = IndefiniteIntervalT.indefiniteIntervalTBehavior[Int]
  val i1 = IndefiniteIntervalT(1,Some(5))
  val i2 = IndefiniteIntervalT(2,Some(4))
  val i3 = IndefiniteIntervalT(3,Some(7))
  val i4 = IndefiniteIntervalT(3,Some(6))
  val i5 = IndefiniteIntervalT(3,None)
  val i6 = IndefiniteIntervalT(7,Some(10))

  @Test
  def testIndefiniteIntervalOrdering {
    val intervals = List(i3, i2, i1, i5, i4)
    val sorted = intervals.sorted(indefiniteIntervalOrdering[Int])

    assertEquals(List(i1, i2, i4, i3, i5), sorted)
  }

  @Test
  def testIntersect {
    assertEquals(Some(i2), intersect(i1, i2))
    assertEquals(Some(IndefiniteIntervalT(3, Some(5))), intersect(i1, i3))
    assertTrue(intersect(i1, i6).isEmpty)
    assertEquals(Some(IndefiniteIntervalT(7, Some(7))), intersect(i3,i6))
  }

  @Test
  def testUnion {
    assertEquals(Left(i1), union(i1, i2))
    assertEquals(Left(IndefiniteIntervalT(1,Some(7))), union(i1, i3))
    assertEquals(Right(i1, i6), union(i1, i6))
    assertEquals(Left(IndefiniteIntervalT(3, Some(10))), union(i3, i6))
  }

  @Test
  def testIsInfinite {
    assertFalse(isInfinite(i1))
    assertTrue(isInfinite(i5))
  }

  @Test
  def testContains{
    for (i <- 1 to 4) {
      assertTrue("did not contain " + i, contains(i1, i))
    }
    for (i <- -100 to 0) {
      assertFalse(contains(i1, i))
    }
    for (i <- 5 to 100) {
      assertFalse(contains(i1, i))
    }


    for (i <- 3 to 100) {
      assertTrue(contains(i5, i))
    }

    for (i <- -100 to 2) {
      assertFalse(contains(i5, i))
    }

  }

  @Test
  def testMerge {
    val m1 = merge(List(i1, i2, i3, i4, i5, i6))
    assertEquals(List(IndefiniteIntervalT(1, None)), m1)

    val m2 = merge(List(i4, i6))
    assertEquals(List(i4, i6), m2)

  }




}
