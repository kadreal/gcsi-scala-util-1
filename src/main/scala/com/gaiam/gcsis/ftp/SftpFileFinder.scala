/*
 * SftpFileFinder.scala
 * Copyright Gaiam Inc.
 */

package com.gaiam.gcsis.ftp

import com.jcraft.jsch._
import java.util.Vector
import scalaj.collection.Imports._
import com.google.inject.Inject


/**
*
*/
class SftpFileFinder @Inject() (val channel: ChannelSftp, rootPath: String, pattern: String) {
    type LsEntry = ChannelSftp#LsEntry
    
    def poll : Seq[String] = {
        channel.ls(rootPath).asScala.filter( _ match {
                case e: LsEntry => pattern.r.findFirstIn(e.getFilename).isDefined
        }).flatMap( _ match {
            case e: LsEntry => Some(rootPath + "/" + e.getFilename)
            case s => None
        })
    }
}
