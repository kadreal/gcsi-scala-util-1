package com.gaiam.gcsis.util

/**
 * User: tstevens
 * Date: 11/17/11
 * If Both are None = 0
 * If Some,None = 1
 * If None,Some = -1
 * If Some,Some then compare value
 */

object OptionOrdered {
  
  def create[T](noneOrdering: NoneOrdering, f: (T, T) => Int): Ordering[Option[T]] = {
    val ordering = new Ordering[T]() {
      def compare(x: T, y: T) = f(x,y)
    }
    OptionOrdered(noneOrdering)(ordering)
  }

  def apply[T](no: NoneOrdering)(implicit ordering: Ordering[T]) = {
    new OptionOrdered[T] {
      val u = ordering
      val noneOrdering = no
    }
  }

  implicit def toOptionOrdered[T](implicit ordering: Ordering[T]): OptionOrdered[T] = {
    new OptionOrdered[T] {
      val u = ordering
      val noneOrdering = NoneLast()
    }
  }
}

trait NoneOrdering {
  def noneSome: Int
  def someNone: Int
}
case class NoneFirst() extends NoneOrdering{
  val noneSome = -1
  val someNone = 1
}
case class NoneLast() extends NoneOrdering {
  val noneSome = 1
  val someNone = -1
}

trait OptionOrdered[T] extends Ordering[Option[T]]{
  val u: Ordering[T]
  val noneOrdering: NoneOrdering

  def compare(x: Option[T], y: Option[T]) = {
    if (x.isEmpty && y.isEmpty) 0
    else if (x.isDefined && y.isEmpty) 1
    else if (x.isEmpty && y.isDefined) -1
    else {
      x.flatMap(first => {
        y.map(second => {
          u.compare(first,second)
        })
      }).getOrElse(0)
    }
  }
}

