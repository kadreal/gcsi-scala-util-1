package com.gaiam.gcsis.ftp

import java.io.InputStream

/**
 * Implementations should create the specified file with the specified output, somewhere.
 * @author tstevens
 */
trait FileDelivery {
    /**
     * @param out The callback which will write the appropriate data to the file.
     */
    def deliver(relativePath: String, out: FileGenerator): Unit

}

trait FileRetrieval {
  def retrieve[T](remoteFile: String)(f: InputStream => T): T
}