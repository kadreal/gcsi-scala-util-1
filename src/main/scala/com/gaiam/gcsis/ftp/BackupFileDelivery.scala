package com.gaiam.gcsis.ftp;


import com.gaiam.gcsis.util.FS._
import com.gaiam.gcsis.util.OutputStreams._
import com.gaiam.gcsis.util.Logging.logger
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.InputStream

/**
 *
 * @author tstevens
 */
class BackupFileDelivery(delegate: FileDelivery, val backupDir: File) extends FileDelivery {
    val log = logger[BackupFileDelivery]

    /**
     * Tries to write to the filesystem and tries to write to the ftp server. If both fail, an exception is thrown. If
     * one fails, the error is logged, but an exception is not thrown.
     */
    def deliver(remotePath: String, out: FileGenerator): Unit = {
        if (!(backupDir.exists || backupDir.mkdirs)) {
            log.error("Could not make backup directories:" + this.backupDir.getAbsolutePath());
        }

        val file = new File(this.backupDir, out.fileName);
        var primaryErrors: List[Exception] = Nil
        delegate.deliver(
            remotePath,
            new FileGenerator {
                def fileName = out.fileName
                def write(os: OutputStream): Unit = {
                    val primary = safe(os)
                    val backup = safe(new FileOutputStream(file))
                    using(backup) {
                        fos => out.write(dup(fos, primary))
                    }

                    (primary.err, backup.err) match {
                        case (None, None) => ()
                        case (Some(e), None) =>
                            log.error("Could not deliver the file '" + fileName + "'. This must be delivered manually! ", e)

                        case (None, Some(e)) =>
                            log.error("Could not back up the file '" + fileName + "' to the filesystem. ", e)

                        case (Some(pe), Some(be)) =>
                            log.error("Both primary file delivery and backup of file '" + fileName + "' to the filesystem failed!")
                            log.error("Primary delivery of file '" + fileName + "' failed!", pe)
                            log.error("Backup of file '" + fileName + "' failed!", be);
                            throw pe
                    }
                }
            }
        )
    }

}
