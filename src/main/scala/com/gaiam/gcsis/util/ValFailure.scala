package com.gaiam.gcsis.util

/**
 * Validation failures that mimick logging.
 * User: tstevens
 * Date: 11/22/11
 */
trait ValResult {
  def message: String
}

/** If the validation failed and something or someone needs to be told. */
case class ValFatal(val message: String) extends ValResult

/** If validation failed, but nobody needs to be modified. */
case class ValInfo(val message: String) extends ValResult