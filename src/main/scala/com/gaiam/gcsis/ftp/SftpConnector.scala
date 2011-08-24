/*
 * SftpConnect.scala
 * Copyright Gaiam Inc.
 */

package com.gaiam.gcsis.ftp

import com.jcraft.jsch.ChannelSftp
import com.jcraft.jsch.JSch


class SshEnvironment(val knownHosts: String, val privateKeyFile: String, val publicKeyFile: String)

object SshEnvironment {
    def defaultDsa = {
        val home = System.getProperty("user.home")
        new SshEnvironment(home + "/.ssh/known_hosts", home + "/.ssh/id_dsa", home + "/.ssh/id_dsa.pub")
    }

    def gcsi = {
        val home = System.getProperty("user.home")
        new SshEnvironment(home + "/.ssh/known_hosts", home + "/.ssh/gcsi_id_dsa", home + "/.ssh/gcsi_id_dsa.pub")

    }
}

trait ChannelConnector {
   def withChannel[T](host:String, userName: String)(f : (ChannelSftp) => T) : T
   def withChannel[T](host: String, userName: String, port: Int)( f : (ChannelSftp) => T) : T
}

/** Facilitates opening a ChannelSftp session. */
class SftpConnector(val env: SshEnvironment, val passphrase: String) extends ChannelConnector {


    val jsch = new JSch()
    jsch.addIdentity(env.privateKeyFile, env.publicKeyFile, passphrase.getBytes)
    jsch.setKnownHosts(env.knownHosts)

    def withChannel[T](host:String, userName: String)(f : (ChannelSftp) => T) : T = {
        withChannel(host, userName, 22)(f)
    }

    def withChannel[T](host: String, userName: String, port: Int)( f : (ChannelSftp) => T) : T = {
        val session = jsch.getSession(userName, host, port)
        session.setConfig("PreferredAuthentications","publickey,keyboard-interactive,password")
        session.connect
        val channel = session.openChannel("sftp").asInstanceOf[ChannelSftp]
        channel.connect

        try {
            f(channel)
        } finally {
            channel.disconnect
            session.disconnect
        }

    }

}
