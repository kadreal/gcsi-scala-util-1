package com.gaiam.gcsis.ftp

import com.gaiam.gcsis.util.FS._
import java.io._
import java.net.URL
import org.junit.{Ignore, Test}


/**
 *
 * @author tstevens
 */
class OnlineFileDeliveryTest {
    def createDelivery = new FtpFileDelivery(new URL("ftp://mockftp:gfctspi@carrot"));

    @Test
    @Ignore
    def testWrite {
        createDelivery.deliver(
            "../../tmp",
            new FileGenerator {
                val fileName = "test-out.txt"
                def write(os: OutputStream): Unit = {
                    using(new PrintWriter(os)) {
                        _.println("Hello World!")
                    } 
                }
            }
        )
    }
}
