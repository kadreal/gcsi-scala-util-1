/*
 * DupOutputStream.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.gaiam.gcsis.util

import java.io.OutputStream
import scala.collection.mutable._

object OutputStreams {
    def dup(o1: OutputStream, o2: OutputStream) = new DupOutputStream(o1, o2)
    def safe(out: OutputStream) = new SafeOutputStream(out)
}

class DupOutputStream(o1: OutputStream, o2: OutputStream) extends OutputStream {
    override def write(b: Int) = {
        o1.write(b)
        o2.write(b)
    }

    override def write(b: Array[Byte]) = {
        o1.write(b)
        o2.write(b)
    }

    override def write(b: Array[Byte], off: Int, len: Int) = {
        o1.write(b, off, len)
        o2.write(b, off, len)
    }

    override def flush: Unit = {
        o1.flush
        o2.flush
    }

    override def close: Unit = {
        o1.close
        o2.close
    }
}

/**
 * This OutputStream will never throw an exception, but will capture the first 
 * exception thrown by any operation on the wrapped stream.
 */
class SafeOutputStream(out: OutputStream) extends OutputStream {
    private var _err: Option[Exception] = None
    def err = _err

    override def write(b: Int) = {
        if (err == None) try { out.write(b) } catch { case e: Exception => _err = Some(e) }
    }

    override def write(b: Array[Byte]) = {
        if (err == None) try { out.write(b) } catch { case e: Exception => _err = Some(e) }
    }

    override def write(b: Array[Byte], off: Int, len: Int) = {
        if (err == None) try { out.write(b, off, len) } catch { case e: Exception => _err = Some(e) }
    }

    override def flush: Unit = {
        if (err == None) try { out.flush } catch { case e: Exception => _err = Some(e) }
    }

    override def close: Unit = {
        if (err == None) try { out.close } catch { case e: Exception => _err = Some(e) }
    }
}
