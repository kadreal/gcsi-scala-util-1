package com.gaiam.gcsis.util

import com.gaiam.gcsis.util.FS._
import com.gaiam.gcsis.util.FS.implicits._
import java.io.FileInputStream
import java.io.InputStream
import java.io.OutputStream
import java.security.SecureRandom
import java.security.Security
import java.util.Date
import java.util.Iterator
import java.lang.{Long => JLong}
import scalaj.collection.Imports._

import org.bouncycastle.bcpg.BCPGOutputStream
import org.bouncycastle.bcpg.CompressionAlgorithmTags
import org.bouncycastle.bcpg.HashAlgorithmTags
import org.bouncycastle.bcpg.PublicKeyAlgorithmTags
import org.bouncycastle.bcpg.SymmetricKeyAlgorithmTags
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.PGPCompressedDataGenerator
import org.bouncycastle.openpgp.PGPEncryptedDataGenerator
import org.bouncycastle.openpgp.PGPLiteralDataGenerator
import org.bouncycastle.openpgp.PGPPublicKey
import org.bouncycastle.openpgp.PGPPublicKeyRing
import org.bouncycastle.openpgp.PGPPublicKeyRingCollection
import org.bouncycastle.openpgp.PGPSecretKey
import org.bouncycastle.openpgp.PGPSecretKeyRing
import org.bouncycastle.openpgp.PGPSecretKeyRingCollection
import org.bouncycastle.openpgp.PGPSignature
import org.bouncycastle.openpgp.PGPSignatureGenerator
import org.bouncycastle.openpgp.PGPSignatureSubpacketGenerator
import org.bouncycastle.openpgp.PGPUtil

object EncryptedSignedOutputStream {
    Security.addProvider(new BouncyCastleProvider)

    def apply(
        out: OutputStream,
        fileName: String,
        pubringFile: String,
        secringFile: String,
        partialEncryptionKeyId: String,
        partialSignatureKeyId: String,
        password: String
    ) = new EncryptedSignedOutputStream(
        out,
        fileName,
        readPublicKey(new FileInputStream(pubringFile), partialEncryptionKeyId),
        readSecretKey(new FileInputStream(secringFile), partialSignatureKeyId),
        password
    )

    /**
     * Responsible for creating a FileOutputStream, in that anything that is written to it is encrypted and signed using
     * OpenPGP in memory, before it is written to the file.
     * 
     * @param publicKeyringStream
     *            Probably the file input stream of the public key ring.
     * @param secretKeyringStream
     *            Probably the file input stream of the secret key ring.
     * @param partialEncryptionKeyId
     *            This is a partial hex string of the id of the key to use to encrypt. This can be found by running
     *            <code>gpg --list-keys</code> on the keyring file. It is the after the first '/'.
     * @param partialSignatureKeyId
     *            This is the partial hex string for the id of the key to use to sign.
     * @password The password used to unlock the private encryption key for signing.
     */
    def apply(
        out: OutputStream,
        fileName: String,
        publicKeyringStream: InputStream,
        secretKeyringStream: InputStream,
        partialEncryptionKeyId: String,
        partialSignatureKeyId: String,
        password: String
    ) = new EncryptedSignedOutputStream(
        out,
        fileName,
        readPublicKey(publicKeyringStream, partialEncryptionKeyId),
        readSecretKey(secretKeyringStream, partialSignatureKeyId),
        password
    )

    def readPublicKey(publicKeyringStream: InputStream, partialEncryptionKeyId: String): PGPPublicKey = {
        using(PGPUtil.getDecoderStream(publicKeyringStream)) {
            pubIn => readPublicKey(new PGPPublicKeyRingCollection(pubIn), partialEncryptionKeyId)
        }
    }

    /**
     * Find the specified public key in the KeyRingCollection.
     * 
     * @param ringCollection
     *            Look for the key in here.
     * @param partialKeyId
     *            If the key id contains this id, return that key.
     * @return
     */
    def readPublicKey(keyRings: PGPPublicKeyRingCollection, partialKeyId: String): PGPPublicKey = {
        val keys = for {
            keyRing <- keyRings.getKeyRings.asInstanceOf[Iterator[PGPPublicKeyRing]].asScala
            key <- keyRing.getPublicKeys.asInstanceOf[Iterator[PGPPublicKey]].asScala
            if JLong.toHexString(key.getKeyID).toUpperCase.endsWith(partialKeyId)
        } yield key

        if (keys.hasNext) {
            keys.next
        } else {
            throw new IllegalStateException("Can't find encryption key in key ring with id " + partialKeyId + ".")
        }
    }

    def readSecretKey(secretKeyringStream: InputStream, partialSignatureKeyId: String): PGPSecretKey = {
        using(PGPUtil.getDecoderStream(secretKeyringStream)) {
            secIn => readSecretKey(new PGPSecretKeyRingCollection(secIn), partialSignatureKeyId)
        }
    }

    /**
     * Find the specified public key in the KeyRingCollection.
     * 
     * @param pgpSec
     *            Look for the key in here.
     * @param partialKeyId
     *            If the key id contains this id, return that key.
     * @return
     */
    def readSecretKey(keyRings: PGPSecretKeyRingCollection, partialKeyId: String): PGPSecretKey = {
        val keys = for {
            keyRing <- keyRings.getKeyRings.asInstanceOf[Iterator[PGPSecretKeyRing]].asScala
            key <- keyRing.getSecretKeys.asInstanceOf[Iterator[PGPSecretKey]].asScala
            if JLong.toHexString(key.getKeyID).toUpperCase.endsWith(partialKeyId)
        } yield key

        if (keys.hasNext) {
            keys.next
        } else {
            throw new IllegalStateException("Can't find encryption key in key ring with id " + partialKeyId + ".")
        }
    }
}

/**
 * An OutputStream which signs & encrypts the input data before sending it to the
 * delegate stream. This class is not threadsafe due to the way that the PGP
 * signature is written! The signature is written only when the stream is finally
 * closed.
 */
class EncryptedSignedOutputStream(os: OutputStream, fileName: String, encryptionKey: PGPPublicKey, signatureKey: PGPSecretKey, password: String, bufSize: Int) extends OutputStream {

    /**
     * This constructor will use a default buffer of (1 << 16) bytes
     */
    def this(os: OutputStream, fileName: String, encryptionKey: PGPPublicKey, signatureKey: PGPSecretKey, password: String) = {
        this(os, fileName, encryptionKey, signatureKey, password, 1 << 16)
    }

    // init signature generator
    val privateKey = signatureKey.extractPrivateKey(password.toCharArray, "BC")
    val hashAlgorithm = if (signatureKey.getKeyEncryptionAlgorithm == 0) PublicKeyAlgorithmTags.DSA else signatureKey.getKeyEncryptionAlgorithm 
    val sigGen = new PGPSignatureGenerator(hashAlgorithm, "BC", HashAlgorithmTags.SHA1, "BC") ->- {
        gen => {
            gen.initSign(PGPSignature.BINARY_DOCUMENT, privateKey)

            val userIds = signatureKey.getUserIDs.asInstanceOf[Iterator[String]]
            if (userIds.hasNext) {
                val subpacketGen = new PGPSignatureSubpacketGenerator
                subpacketGen.setSignerUserID(false, userIds.next)
                gen.setHashedSubpackets(subpacketGen.generate)
            }
        }
    }

    // open cipher stream
    val cipherStreamGen = new PGPEncryptedDataGenerator(SymmetricKeyAlgorithmTags.CAST5, true, new SecureRandom, "BC") ->- (_.addMethod(encryptionKey))
    val cipherStream = cipherStreamGen.open(os, new Array[Byte](bufSize))

    //open compression stream
    val compressionGen = new PGPCompressedDataGenerator(CompressionAlgorithmTags.ZIP)
    val compressionStream = new BCPGOutputStream(compressionGen.open(cipherStream))

    //write signature header to compression stream
    sigGen.generateOnePassVersion(false).encode(compressionStream)

    val literalPacketGen = new PGPLiteralDataGenerator
    val packetStream = literalPacketGen.open(compressionStream, PGPLiteralDataGenerator.BINARY, this.fileName, new Date, new Array[Byte](bufSize))

    @volatile private var open = true

    override def write(b: Int): Unit = {
        packetStream.write(b)
        sigGen.update(b.toByte)
    }

    override def write(b: Array[Byte]): Unit = {
        packetStream.write(b)
        sigGen.update(b)
    }

    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
        packetStream.write(b, off, len)
        sigGen.update(b, off, len)
    }

    override def flush(): Unit = {
        packetStream.flush
        compressionStream.flush
        cipherStream.flush
    }

    /**
     * When the user closes the stream, sign and close the underlying streams.
     */
    override def close {
        if (open) {
            packetStream.close

            //write final signature to the compression stream
            sigGen.generate.encode(compressionStream)
            compressionStream.close

            cipherStream.close
            os.close
            open = false
        }
    }
}
