package com.gaiam.gcsis.ftp;

import java.io.OutputStream;

/**
 * This interface represents a generator for file contents. 
 * 
 * See also {@link FtpFileDelivery}.
 * @author Travis Stevens@gaiam.com
 * @see FtpFileDelivery
 *
 */
trait FileGenerator {
    /**
     * Return the name of the file to be written
     */
    def fileName: String

    /**
     * If finalName is specified, then upon completion of writing 
     * the file to a target system the file will be renamed to this finalName.
     * This is useful for targets like the Paymentech batch system where a change
     * to a prefix of the filename is used to indicate that a file transfer
     * is complete and that the file is ready for processing.
     */
    def finalName: Option[String] = None

    /**
     * Write the file contents to the stream.
     * 
     * @param os - The stream to write to.
     */
    def write(os: OutputStream)
}
