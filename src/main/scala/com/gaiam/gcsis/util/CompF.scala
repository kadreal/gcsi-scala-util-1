package com.gaiam.gcsis.util

/**
 * User: tstevens
 * Date: 10/31/12
 */
object CompF {
  /**
   * Compare two optional values, either of which may not exist. None
   * values are interpreted to be at positive infinity.
   */
  def comparePositive[T](a: Option[T], b: Option[T])(implicit ord: Ordering[T]): Int = {

    (a,b) match {
      case (None, None) => 0
      case (None, _) => 1
      case (_, None) => -1
      case (Some(a),Some(b)) => ord.compare(a,b)
    }
  }

  case class OptionalOrdering[T:Ordering]() extends Ordering[Option[T]] {
    def compare(x: Option[T], y: Option[T]) = comparePositive(x,y)
  }

  class OptionalOrdering2[T]()(implicit o: Ordering[T]) extends Ordering[Option[T]] {
    def compare(x: Option[T], y: Option[T]) = comparePositive(x,y)
  }
}
