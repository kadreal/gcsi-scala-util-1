package com.gaiam.gcsis.util

import scalaz.{Order, Equal, Show}
import scalaz.Ordering

/**
 * User: travis.stevens@gaiam.com
 * Date: 12/20/13
 */
trait UuidInstances {
  import java.util.UUID

  implicit object uuidInstance extends Show[UUID] with Equal[UUID] with Order[UUID] {

    def order(x: UUID, y: UUID) =  Ordering.fromInt(scala.math.Ordering.String.compare(x.toString, y.toString))

    override def show(f: UUID) = '"' + f.toString + '"'

    override def equal(x: UUID, y: UUID) = x == y

    override def equalIsNatural: Boolean = true

  }

}

object uuid extends UuidInstances
