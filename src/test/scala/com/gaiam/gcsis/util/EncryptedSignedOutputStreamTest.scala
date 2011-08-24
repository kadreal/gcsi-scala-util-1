package com.gaiam.gcsis.util

import com.gaiam.gcsis.util.FS._
import com.gaiam.gcsis.util.FS.implicits._
import java.io._
import org.bouncycastle.openpgp._
import scalaj.collection.Imports._

import org.junit.Ignore;
import org.junit.Test;
import org.junit.Assert._

class EncryptedSignedOutputStreamTest {
    val encryptionId = "DB140A74"
    val signersId = "A73C9614"
    val password = "We love yoga!"

    val payload = new StringBuffer ->* {
        buf =>
        for (i <- 0 to 20) {
            buf.append("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz\n");
        }
        buf.toString
    }

    /**
     * This is just a simple test that I used when clearing up the fact that maven was
     * reencoding the keyring during its process resources phase. Thought I'd leave
     * it around for posterity.
     */
    @Test
    @Ignore
    def testOpenKeyring {
        val publicKeyringStream = Thread.currentThread().getContextClassLoader().getResourceAsStream("keyring/pubring")
        val pubIn = PGPUtil.getDecoderStream(publicKeyringStream);
        val pgpPub = new PGPPublicKeyRingCollection(pubIn);

        assertNotNull(pgpPub)
    }

    @Test
    @Ignore
    def testWrite {
        val encrypted = using(Thread.currentThread().getContextClassLoader().getResourceAsStream("keyring/pubring")) {
            pubring => using(Thread.currentThread().getContextClassLoader().getResourceAsStream("keyring/secring")) {
                secring =>

                assertNotNull(pubring)
                assertNotNull(secring)

                using(new ByteArrayOutputStream(8192)) {
                    out => {
                        using(EncryptedSignedOutputStream(out, "crypt-test", pubring, secring, encryptionId, signersId, password)) {
                            wos => using(new PrintWriter(wos)) {
                                _.print(payload)
                            }
                        }

                        out.toByteArray
                    }
                }
            }
        }
        
        assertTrue(encrypted.length > 0)

        var decrypted: String = null
        
        using(Thread.currentThread().getContextClassLoader().getResourceAsStream("keyring/remote-keyring")) {
            pubring => using(Thread.currentThread().getContextClassLoader().getResourceAsStream("keyring/remote-secret-keyring")) {
                secring =>
        
                val pubIn = PGPUtil.getDecoderStream(pubring);
                val pgpPub = new PGPPublicKeyRingCollection(pubIn);
                val signatureKey = EncryptedSignedOutputStream.readPublicKey(pgpPub, signersId);
        
                val secIn = PGPUtil.getDecoderStream(secring);
                val pgpSec = new PGPSecretKeyRingCollection(secIn);
        
                def decode(fac: PGPObjectFactory): Unit = fac.nextObject match {
                    case null => ()
                    case dataList: PGPEncryptedDataList =>
                        val encObjects = dataList.getEncryptedDataObjects.asScala
                        assertTrue(encObjects.hasNext)

                        for (obj <- encObjects) {
                            obj match {
                                case pbe: PGPPublicKeyEncryptedData =>
                                    val decryptionKey = pgpSec.getSecretKey(pbe.getKeyID)
                                    assertNotNull(decryptionKey)

                                    val privateKey = decryptionKey.extractPrivateKey(password.toCharArray, "BC");
                                    assertNotNull(privateKey)

                                    using(pbe.getDataStream(privateKey, "BC")) {
                                        clear => decode(new PGPObjectFactory(clear))
                                    }

                                case _ => throw new IllegalStateException("Could not find encrypted data")
                            }
                        }
                        decode(fac)

                    case cdata: PGPCompressedData =>
                        using(cdata.getDataStream) {
                            cstream => decode(new PGPObjectFactory(cstream))
                        }
                        decode(fac)

                    case ldata: PGPLiteralData =>
                        val fn = ldata.getFileName
                        using(ldata.getInputStream) {
                            lstream => {
                                val buf = new Array[Byte](8192)
                                val out = new ByteArrayOutputStream(8192)
                                var read = 0
                                while(read > -1) {
                                    read = lstream.read(buf)
                                    if (read > -1) out.write(buf, 0, read)
                                }

                                decrypted = new String(out.toByteArray)
                            }
                        }
                        decode(fac)

                    case sig: PGPOnePassSignatureList =>
                        decode(fac)

                    case x =>
                        decode(fac)
                }

                using(PGPUtil.getDecoderStream(new ByteArrayInputStream(encrypted))) {
                    cipherStream => decode(new PGPObjectFactory(cipherStream))
                }
            }
        }

        assertEquals(payload, decrypted);
    }
}