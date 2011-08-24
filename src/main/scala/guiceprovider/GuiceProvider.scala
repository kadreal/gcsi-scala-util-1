package com.guiceprovider

import com.google.inject.{Key, Injector}
import scalaz._
import com.google.inject.name.Names

/**
 * User: travis.stevens@gaiam.com
 * Date: 12/4/13
 */
trait GuiceProvider[T] {
  def apply[U](r: Reader[T,U]): U
}

object GuiceProvider {


  implicit class RichInjector(inj: Injector) {

    /** Convenience method for inj.getInstance(classOf[T]) */
    def instance[T:Manifest] = inj.getInstance(implicitly[Manifest[T]].runtimeClass).asInstanceOf[T]

    /** Convenience method for inj.getInstance(Key.get(classOf[T], Names.named(named)) */
    def instance[T:Manifest](named: String): T = inj.getInstance(Key.get(implicitly[Manifest[T]].runtimeClass, Names.named(named))).asInstanceOf[T]

    def buildProvider[T](f: Injector => T) : GuiceProvider[T] = {
      new GuiceProvider[T] {
        def apply[U](r: Reader[T, U]) = r.apply(f(inj))
      }
    }

    /** Provider with 1 parameter */
    def provider[T:Manifest] = new GuiceProvider[T] {
      def apply[A](r: Reader[T,A]) : A = {
        r.apply(inj.getInstance(implicitly[Manifest[T]].runtimeClass).asInstanceOf[T])
      }
    }


    /** Provider with 2 parameters */
    def provider[T:Manifest, U:Manifest] = new GuiceProvider[(T,U)] {
      def apply[A](r: Reader[(T,U),A]) : A = {
        r.apply(
          (inj.getInstance(implicitly[Manifest[T]].runtimeClass).asInstanceOf[T],
            inj.getInstance(implicitly[Manifest[U]].runtimeClass).asInstanceOf[U])
        )
      }
    }

    /** Provider with 3 parameters */
    def provider[T:Manifest, U:Manifest, V:Manifest] = new GuiceProvider[(T,U,V)] {
      def apply[A](r: Reader[(T,U,V),A]) : A = {
        r.apply(
          (inj.getInstance(implicitly[Manifest[T]].runtimeClass).asInstanceOf[T],
            inj.getInstance(implicitly[Manifest[U]].runtimeClass).asInstanceOf[U],
            inj.getInstance(implicitly[Manifest[V]].runtimeClass).asInstanceOf[V])
        )
      }
    }

    /** Provider with 4 parameters */
    def provider[T:Manifest, U:Manifest, V:Manifest, W:Manifest] = new GuiceProvider[(T,U,V,W)] {
      def apply[A](r: Reader[(T,U,V,W),A]) : A = {
        r.apply(
          (inj.getInstance(implicitly[Manifest[T]].runtimeClass).asInstanceOf[T],
            inj.getInstance(implicitly[Manifest[U]].runtimeClass).asInstanceOf[U],
            inj.getInstance(implicitly[Manifest[V]].runtimeClass).asInstanceOf[V],
            inj.getInstance(implicitly[Manifest[W]].runtimeClass).asInstanceOf[W])
        )
      }
    }
    /** Provider with 4 parameters */
    def provider[T:Manifest, U:Manifest, V:Manifest, W:Manifest, X:Manifest] = new GuiceProvider[(T,U,V,W,X)] {
      def apply[A](r: Reader[(T,U,V,W,X),A]) : A = {
        r.apply(
          (inj.getInstance(implicitly[Manifest[T]].runtimeClass).asInstanceOf[T],
            inj.getInstance(implicitly[Manifest[U]].runtimeClass).asInstanceOf[U],
            inj.getInstance(implicitly[Manifest[V]].runtimeClass).asInstanceOf[V],
            inj.getInstance(implicitly[Manifest[W]].runtimeClass).asInstanceOf[W],
            inj.getInstance(implicitly[Manifest[X]].runtimeClass).asInstanceOf[X])
        )
      }
    }
  }
}
