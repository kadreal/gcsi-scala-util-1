package com.gaiam.gcsis.util

/**
 * An indefinite interval over ordered values, where either start and/or end
 * may be undefined; such intervals have infinite domain in the positive 
 * and/or negative directions.
 */
case class Interval[A <% Ordered[A]](val start: Option[A], val end: Option[A]) extends Ordered[Interval[A]] {
    def withStart(start: Option[A]): Interval[A] = Interval(start, end)

    def withEnd(end: Option[A]): Interval[A] = Interval(start, end)

    def startsBefore(other: Interval[A]): Boolean = (start, other.start) match {
        case (_, None) => false
        case (Some(t), Some(o)) => t < o
        case _ => true
    }

    def endsAfter(other: Interval[A]): Boolean = (end, other.end) match {
        case (_, None) => false
        case (Some(t), Some(o)) => t > o
        case _ => true
    }

    def disjoint(other: Interval[A]): Boolean = start.exists(s => other.end.exists(e => e < s)) || other.start.exists(s => end.exists(e => s > e))

    def intersect(other: Interval[A]): Option[Interval[A]] = {
        if (disjoint(other)) {
            None
        } else if (this.compare(other) == 0 || (startsBefore(other) && endsAfter(other))) {
            Some(other) //other is equal to or wholly contained in this interval
        } else if (startsBefore(other)) {
            Some(Interval(other.start, end))
        } else if (endsAfter(other)) {
            Some(Interval(start, other.end))
        } else {
            other.intersect(this)
        }
    }

    def union(other: Interval[A]): Either[Interval[A], (Interval[A], Interval[A])] = {
        if (disjoint(other)) {
            Right((this, other))
        } else if (this.compare(other) == 0 || (startsBefore(other) && endsAfter(other))) {
            Left(this) //other is equal to or wholly contained in this interval
        } else if (startsBefore(other)) {
            Left(Interval(start, other.end))
        } else if (endsAfter(other)) {
            Left(Interval(other.start, end))
        } else {
            other.union(this)
        }
    }

    def compare(other: Interval[A]): Int = {
        if (startsBefore(other)) -1
        else if (other.startsBefore(this)) 1
        else if (endsAfter(other)) 1
        else if (other.endsAfter(this)) -1
        else 0
    }
}
