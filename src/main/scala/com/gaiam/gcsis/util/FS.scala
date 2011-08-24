/*
 * FS.scala
 */

package com.gaiam.gcsis.util

import java.io.Closeable
import java.io.StringWriter
import java.io.PrintWriter
import scala.collection.Set
import scala.collection.SortedSet
import com.gaiam.gcsis.util.Logging._

object FS {
    trait Implicits {
        implicit def Kestrel[A](t: A): Kestrel[A] = new Kestrel[A](t)

        implicit def Thrush[A](t: A): Thrush[A] = new Thrush[A](t)

        implicit def Filter[A](a: A): Filter[A] = new Filter[A](a)

        implicit def FSRichOption[A](o: Option[A]): FSRichOption[A] = new FSRichOption(o)

        implicit def FSRichIterable[A](i: Iterable[A]): FSRichIterable[A] = new FSRichIterable(i)

        implicit def FSRichMap[A, B](m: Map[A, B]): FSRichMap[A, B] = new FSRichMap(m)

        implicit def FSRichSet[A](s: Set[A]) = new FSRichSet(s)

        implicit def FSRichException(e: Exception): FSRichException = new FSRichException(e)

        implicit def FSRichFunction[A,B](f: A => B) = new FSRichFunction(f)
    }

    object implicits extends Implicits

    def using[A <: Closeable, U](resource: A)(f: A => U): U = try {
        f(resource)
    } finally {
        resource.close
    }

    /**
     * If an exception is thrown in the execution of the function, report the
     * exception and return None.
     *
     * Usage:
     *
     * val log = Logger
     *
     * exceptionToNone(log.error("Could not count to 42", _)) {
     *  anOption.map(_.calculateValue)
     * }
     *
     * If _.calculateValue throws an exception, it will be reported and None will be returned,
     * otherwise the option resulting from the fuction will be returned.
     *
     */
    def exceptionToNone[A, T <: Exception](l: (Exception) => Unit, allowable: List[Class[T]] = List(classOf[Exception]))(f: => Option[A]) : Option[A] = {
      try {
        f
      } catch {
        case ex: Exception if (allowable.exists(_.isAssignableFrom(ex.getClass))) => { l(ex); None }
      }
    }

    def exceptionToNone[A, T <: Exception](l: (Exception) => Unit, allowable: Class[T])(f: => Option[A]) : Option[A] = {
      exceptionToNone(l, List(allowable))(f)
    }

    /**
     * If test is true, return the result of f wrapped in a Some.
     */
    def when[A](test: => Boolean)(f: => A): Option[A] = {
        if (test) Some(f) else None
    }

    /** If test is true, return the result of the function.  If
    * test is false, return none.
    */
    def wheno[A](test: => Boolean)(f: => Option[A]): Option[A] = {
        if (test) f else None
    }

    class Filter[A](v: A) {
        def filter(f: A => Boolean): Option[A] = if (f(v)) Some(v) else None
    }

    class Kestrel[A](t: A) {
        def ->-(f: A => Any): A = { f(t); t }
    }

    class Thrush[A](t: A) {
        def ->*[U](f: A => U): U = f(t)
    }

    class FSRichFunction[A,B](f: A => B) {
        def &&(f2: A => B)(implicit ev: B <:< Boolean): A => Boolean = {
          (a: A) => ev(f(a)) && ev(f2(a))
        }
    }

    class FSRichOption[A](o: Option[A]) {
        def plus(o2: Option[A])(implicit num: Numeric[A]) = o.flatMap(n1 => o2.map(num.plus(n1, _))).orElse(o2)
        def minus(o2: Option[A])(implicit num: Numeric[A]) = o.flatMap(n1 => o2.map(num.minus(n1, _))).orElse(o2)
    }

    class FSRichIterable[A](i: Iterable[A]) {
        private val log = logger("FSRichIterable")
        def findFrom(base: Option[A])(retainCurrent: (A, A) => Boolean): Option[A] = {
            i.foldLeft(base) {
                (current, candidate) => current.filter(retainCurrent(_, candidate)).orElse(Some(candidate))
            }
        }

        def minOption(implicit o: A => Ordered[A]) : Option[A] = i.foldLeft[Option[A]](None) {
            (m, candidate) => m.filter(_ < candidate).orElse(Some(candidate))
        }

        def maxOption(implicit o: A => Ordered[A]) : Option[A] = i.foldLeft[Option[A]](None) {
            (m, candidate) => m.filter(_ > candidate).orElse(Some(candidate))
        }

        def andAll[B](t: Traversable[B]) = i.map(Left(_)) ++ t.map(Right(_))
    }

    class FSRichMap[A, +B](m: Map[A, B]) {
        def updatedWith[B1 >: B](a: A)(f: Option[B] => B1): Map[A,B1] = m + (a -> f(m.get(a)))
    }

    class FSRichException(e: Exception) {
        def stackTrace: String = {
            val w = new StringWriter
            e.printStackTrace(new PrintWriter(w))
            w.toString
        }
    }

    class FSRichSet[A](s: Set[A]) {
        def toSorted(implicit ord: Ordering[A]) = SortedSet(s.toSeq: _*)
    }
}
