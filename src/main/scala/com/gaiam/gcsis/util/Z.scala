package com.gaiam.gcsis.util

import scalaz._

/**
 * User: travis.stevens@gaiam.com
 * Date: 12/20/13
 */
trait ZInstance {

  type EitherReader[X,A,B] = EitherT[({type λ[α] = Reader[X, α]})#λ, A,B]

  def eitherReader[X,A,B](f: X => \/[A,B]): EitherReader[X,A,B] = EitherT[({type λ[+α] = Reader[X, α]})#λ, A,B](Reader(f))

}

object z extends ZInstance
