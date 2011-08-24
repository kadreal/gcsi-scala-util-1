package com.gaiam.gcsis.util

import org.junit.Test
import com.gaiam.gcsis.util.Sha1Generator._
import junit.framework.Assert

/**
 * User: travis.stevens@gaiam.com
 * Date: 11/6/12
 */
class Sha1GeneratorTest {
  @Test
  def testSha1: Unit = {
    val mutton: String = "mutton"
    val result: String = SHA1(mutton)
    Assert.assertEquals("12f65c40449306abbc52419f14d085e2", result)
    val other: String = "other"
    Assert.assertEquals("795f3202b17cb6bc3d4b771d8c6c9eaf", SHA1(other))
  }
}