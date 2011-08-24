package com.gaiam.gcsis.ftp

import java.io.InputStream

/**
 * Implementations should create the specified file with the specified output, somewhere.
 * @author tstevens
 * @reviewed 2008/08/20
 */
trait FileDelivery {
    /**
     * @param remotePath The remote directory path to which the file should be written
     * @param out The callback which will write the appropriate data to the file.
     * @throws IOException if something goes wrong.
     */
    def deliver(relativePath: String, out: FileGenerator): Unit




    def retrieve[T](remoteFile: String)(f: InputStream => T): T
}