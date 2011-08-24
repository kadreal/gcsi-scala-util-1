/*
 * StrUtil.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.gaiam.gcsis.util

object StrUtil {
    /**
     * Splits the strings on lines
     */
    def numl(s: String) = {
        val lines = s.split("\\n")
        val count = lines.size
        val countWidth = (count + ": ").length
        lines.foldLeft((1, List[String]()))({case ((count, list), line) => (count + 1, ("%" + countWidth + "s" + line).format(count + ": ") :: list)})
        ._2.reverse.mkString("","\n","")
    }
}
