package com.gaiam.gcsis.util

import java.util.UUID

/**
 * User: travis.stevens@gaiam.com
 * Date: 6/21/12
 */

object SUUID {
  def fromString(str: String): Option[UUID] = {
    try {
      Some(UUID.fromString(str))
    } catch {
      case e: IllegalArgumentException => {
        None
      }
    }
  }
}
