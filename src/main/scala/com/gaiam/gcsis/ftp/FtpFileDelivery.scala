package com.gaiam.gcsis.ftp;

import com.gaiam.gcsis.util.FS._
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import com.gaiam.gcsis.util.Logging.logger;
import java.io.InputStream


object FtpFileDelivery {
    def apply(username: String, password: String, host: String, dir: String) = {
        new FtpFileDelivery(new URL("ftp://" + username + ":" + password + "@" + host + "/" + dir))
    }
}

/**
 * Class meant to transfer files directly to the biztalk server via FTP.
 * @author tstevens
 */
class FtpFileDelivery(url: URL) extends FileDelivery {
    val log = logger[FtpFileDelivery]

    def deliver(relativePath: String, gen: FileGenerator): Unit = {
        val url = if (relativePath.equals(".")) this.url.toExternalForm else this.url.toExternalForm + "/" + relativePath
        val remoteFile = new URL(url + "/" + gen.fileName)
        val con = remoteFile.openConnection
        con.setConnectTimeout(5000)
        using(con.getOutputStream) {
            gen.write(_)
        }
    }

    override def retrieve[T](remoteFile: String)(f: InputStream => T): T = {
        throw new UnsupportedOperationException
    }
}
