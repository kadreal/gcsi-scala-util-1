package com.gaiam.gcsis.util

import java.security.MessageDigest

/**
 * User: tstevens
 * Date: 10/31/12
 */
object Sha1Generator {
  private[this] def convertToHex2(data: Array[Byte]): String = {

    data.map(d => {
      var hex = Integer.toHexString(d)
      hex = if (hex.length == 1) "0" + hex else hex
      hex.substring(hex.length - 2)
    }).mkString
  }

  def SHA1(text: String): String = {
      val md: MessageDigest = MessageDigest.getInstance("MD5")
      md.update(text.getBytes("UTF-8"))
      val sha1hash: Array[Byte] = md.digest
      convertToHex2(sha1hash)
  }

}
