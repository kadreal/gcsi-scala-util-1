/*
 * Logging.scala
 */

package com.gaiam.gcsis.util

import FS.implicits._
import java.util.logging.Level

/**
 * call-by-name wrappers for log4j.Logger
 */
object Logging {

    def logger(c: Class[_]) = Logger(java.util.logging.Logger.getLogger(c.getName))
    def logger(s: String) = Logger(java.util.logging.Logger.getLogger(s))
    def logger[T](implicit m: scala.reflect.Manifest[T]) = Logger(java.util.logging.Logger.getLogger(m.runtimeClass.getName))

    trait Empty[M[_]] {
      def isEmpty[T](v: M[T]): Boolean;
    }

    implicit def iterableEmpty[M[x] <: Iterable[x]]: Empty[M] = new Empty[M] {
      def isEmpty[T](v: M[T]) = v.isEmpty
    }

    implicit def optionEmpty[M[x] <: Option[x]]: Empty[M] = new Empty[M] {
      def isEmpty[T](v: M[T]) = v.isEmpty
    }

    class ThunkHolder[ T ]( x: => T ) { def eval: T = x }
    implicit def thunkToHolder[ T ]( thunk: => T ) = new ThunkHolder( thunk )

    case class UtilLoggerWrapper(log: java.util.logging.Logger) {
      def isErrorEnabled = log.isLoggable(Level.SEVERE)
      def error = log.log(Level.SEVERE, _: String)
      def error(msg: String, ex: Throwable) = log.log(Level.SEVERE,msg, ex)
      def isWarnEnabled = log.isLoggable(Level.WARNING)
      def warn = log.log(Level.WARNING, _: String)
      def warn(msg: String, ex: Throwable) = log.log(Level.WARNING, msg, ex)
      def isDebugEnabled = log.isLoggable(Level.FINE)
      def debug = log.log(Level.FINE, _: String)
      def debug(msg: String, ex: Throwable) = log.log(Level.FINE, msg, ex)
      def isInfoEnabled = log.isLoggable(Level.INFO)
      def info = log.log(Level.INFO, _: String)
      def info(msg: String, ex: Throwable) = log.log(Level.INFO, msg, ex)
      def isTraceEnabled = log.isLoggable(Level.FINEST)
      def trace = log.log(Level.FINEST, _: String)
      def trace(msg: String, ex: Throwable) = log.log(Level.FINEST, msg, ex)

    }

    case class Logger(log: java.util.logging.Logger) {
        val l = UtilLoggerWrapper(log)

        private def append[T](msg: => String, flag: Boolean, write: String => Unit, f: => T): T = {
            val result = f
            if (flag) write(msg + ": " + result)
            result
        }


        private def appendNone[M[_],T](msg: => String, flag: Boolean, write:String => Unit, f: => M[T], empty: Empty[M]) : M[T] = {
            val result = f
            if (flag && empty.isEmpty(result)) write(msg)
            result
        }

        private def appendIf[T](flag:Boolean, write: String => Unit, c: T => Boolean, f: => (T, ThunkHolder[String])) : T = {
          val (result, msg) = f
          if (flag && c(result)) write(msg.eval)
          result
        }

        def trace(msg: => String) = append(msg, l.isTraceEnabled, l.trace, ())
        def tracev[T](msg: => String)(f: => T) = append(msg, l.isTraceEnabled, l.trace, f)
        def trace(msg: => String, ex: Throwable) = append(msg, l.isTraceEnabled, l.trace(_, ex), ())

        def debug(msg: => String) = append(msg, l.isDebugEnabled, l.debug, ())
        def debugv[T](msg: => String)(f: => T) = append(msg, l.isDebugEnabled, l.debug, f)
        def debug(msg: => String, ex: Throwable) = append(msg, l.isDebugEnabled, l.debug(_, ex), ())
        /** Prints debug message if result is empty (Iterable or Option kinds) */
        def debuge[M[_], T](msg: => String)(f: => M[T])(implicit empty: Empty[M]) = appendNone(msg, l.isDebugEnabled, l.debug(_), f, empty)


        def debugInfo(msg: => String) = append(msg, l.isDebugEnabled, l.info, ())
        def debugInfov[T](msg: => String)(f: => T) = append(msg, l.isDebugEnabled, l.info, f)
        def debugInfo(msg: => String, ex: Throwable) = append(msg, l.isDebugEnabled, l.info(_, ex), ())

        def debugWarn(msg: => String) = append(msg, l.isDebugEnabled, l.warn, ())
        def debugWarnv[T](msg: => String)(f: => T) = append(msg, l.isDebugEnabled, l.warn, f)
        def debugWarn(msg: => String, ex: Throwable) = append(msg, l.isDebugEnabled, l.warn(_, ex), ())

        def debugError(msg: => String) = append(msg, l.isDebugEnabled, l.error, ())
        def debugErrorv[T](msg: => String)(f: => T) = append(msg, l.isDebugEnabled, l.error, f)
        def debugError(msg: => String, ex: Throwable) = append(msg, l.isDebugEnabled, l.error(_, ex), ())

        def info(msg: => String) = append(msg, l.isInfoEnabled, l.info, ())
        def infov[T](msg: => String)(f: => T) = append(msg, l.isInfoEnabled, l.info, f)
        def info(msg: => String, ex: Throwable) = append(msg, l.isInfoEnabled, l.info(_, ex), ())
        /** Prints info message if result of function is empty (Iterable or Option kinds) */
        def infoe[M[_], T](msg: => String)(f: => M[T])(implicit empty: Empty[M])  = appendNone(msg, l.isInfoEnabled, l.info(_), f, empty)
        def infoIf[T](c: T => Boolean)(f: => (T, ThunkHolder[String])) = appendIf(l.isInfoEnabled, l.info, c, f)
        def infoIf[T](c: T => Boolean, msg: => String)(f: => T) = appendIf(l.isInfoEnabled, l.info, c, {(f,msg)})

        def warn(msg: => String) = append(msg, l.isWarnEnabled, l.warn, ())
        def warnv[T](msg: => String)(f: => T) = append(msg, l.isWarnEnabled, l.warn, f)
        def warn(msg: => String, ex: Throwable) = append(msg, l.isWarnEnabled, l.warn(_, ex), ())

        def error(msg: => String) = append(msg, l.isErrorEnabled, l.error, ())
        def errorv[T](msg: => String)(f: => T) = append(msg, l.isErrorEnabled, l.error, f)
        def error(msg: => String, ex: Throwable) = append(msg, l.isErrorEnabled, l.error(_, ex), ())
        def errore[M[_], T](msg: => String)(f: => M[T])(implicit empty: Empty[M])  = appendNone(msg, l.isErrorEnabled, l.error(_), f, empty)

        /**
         * Experimental, not sure if I love this, but it does reduce the syntax.
         * Usage would look like this:
         *
         * val iceCream = log.errorIf((s:String) => s == "Cream Ice"),  {
         *    val ice = "Ice"
         *    val cream = "Cream"
         *    (cream + ice, "Values " + ice + " " + cream " were appended inapropriately"
         * }
         *
         */
        def errorIf[T](c: T => Boolean)(f: => (T, ThunkHolder[String])) = appendIf(l.isErrorEnabled, l.error, c, f)
    }
}
