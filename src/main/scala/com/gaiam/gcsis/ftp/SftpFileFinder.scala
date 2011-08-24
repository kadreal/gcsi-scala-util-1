/*
 * SftpFileFinder.scala
 * Copyright Gaiam Inc.
 */

package com.gaiam.gcsis.ftp

import com.jcraft.jsch._
import scala.collection.JavaConverters._


/**
 * Looks in the rootPath for files that match pattern.
 */
class SftpFileFinder(val channel: ChannelSftp, rootPath: String, pattern: String) {

  def poll: Seq[String] = {
    channel.ls(rootPath).asScala.filter(_ match {
      case e: ChannelSftp#LsEntry => pattern.r.findFirstIn(e.getFilename).isDefined
    }).flatMap(_ match {
      case e: ChannelSftp#LsEntry => Some(rootPath + "/" + e.getFilename)
      case s => None
    })
  }
}
