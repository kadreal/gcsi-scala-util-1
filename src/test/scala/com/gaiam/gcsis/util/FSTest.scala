package com.gaiam.gcsis.util

/*
 * FSTest.scala
 *
 */

import org.junit.Test
import org.junit.Assert._
import FS.implicits._

class FSTest {

    @Test
    def testFlatMapFoldFilter {
        val input = List(1,2,3)
        val result = input.flatMap(i => if (i % 2 == 0) List(-i, -i*2, -i*3) else List(i * 2, i*3)).findFrom(Some(3)) {
            (min, candidate) => min < candidate
        }

        assert(result == Some(-6))
    }

     @Test
    def testFlatMapFoldFilterNone {
        val input = List(1,2,3)
        val result = input.flatMap(i => if (i % 2 == 0) List(-i, -i*2, -i*3) else List(i * 2, i*3)).findFrom(None) {
            (min, candidate) => min < candidate
        }

        assert(result == Some(-6))
    }

}
