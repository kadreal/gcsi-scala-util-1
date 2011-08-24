package com.gaiam.gcsis.ftp;

import com.gaiam.gcsis.util.FS._
import com.gaiam.gcsis.util.FS.implicits._
import com.jcraft.jsch.ChannelSftp;

import java.io._

object SftpFileDelivery {


    /**
    * Encapsilates the connector.
    * Wraps the SftpFileDelivery class in a FileDelivery instance that
    *  does the connecting and disconnecting.
    */
    def createFileDelivery(
        userName: String,
        host: String,
        passphrase: String,
        remotePath: String
    ) =   {

        val connector = new SftpConnector(SshEnvironment.gcsi, passphrase)

        new FileDelivery {
            override def deliver(relativePath: String, gen: FileGenerator) = {
                connector.withChannel(host, userName)(ch => {
                    ch.cd(remotePath)
                    (new SftpFileDelivery(ch)).deliver(relativePath, gen)
                })
            }
            override def retrieve[T](remoteFile: String)(f: InputStream => T): T = {
                connector.withChannel(host, userName)(ch => {
                    new SftpFileDelivery(ch).retrieve(remoteFile)(f)
                })
            }
        }
    }
}

/**
 * In charge of opening a local file and returning the stream so that one can write to the file. It will then attempt to
 * upload that file to a remote FTP location.
 * 
 * @author travis.stevens@gaiam.com
 * @reviewed 2008/08/20
 * 
 */

class SftpFileDelivery(
    channel: ChannelSftp
) extends FileDelivery {
    /**
     * 
     * @param fileName
     * @param output
     * @throws java.io.IOException
     */
    override def deliver(relativePath: String, gen: FileGenerator): Unit = {
        channel.cd(relativePath)
        using(channel.put(gen.fileName)) { out => gen.write(out) }
        gen.finalName.foreach(finalName => channel.rename(gen.fileName, finalName))
    }


    override def retrieve[T](remoteFile: String)(f: InputStream => T): T = {
        using(channel.get(remoteFile)) { in => f(in) }
    }

}
