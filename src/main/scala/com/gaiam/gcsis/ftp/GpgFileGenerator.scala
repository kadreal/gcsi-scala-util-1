/*
 * GpgFileDelivery.scala
 */

package com.gaiam.gcsis.ftp

import com.gaiam.gcsis.util.EncryptedSignedOutputStream

import java.io.InputStream
import java.io.OutputStream

object GpgFileGenerator {
    def apply(
        delegate: FileGenerator,
        pubringFile: String,
        secringFile: String,
        partialEncryptionKeyId: String,
        partialSignatureKeyId: String,
        password: String
    ) = new GpgFileGenerator(
        delegate,
        EncryptedSignedOutputStream(_, fileName(delegate), pubringFile, secringFile, partialEncryptionKeyId, partialSignatureKeyId, password)
    )

    def apply(
        delegate: FileGenerator,
        publicKeyringStream: InputStream,
        secretKeyringStream: InputStream,
        partialEncryptionKeyId: String,
        partialSignatureKeyId: String,
        password: String
    ) = new GpgFileGenerator(
        delegate,
        EncryptedSignedOutputStream(_, fileName(delegate), publicKeyringStream, secretKeyringStream, partialEncryptionKeyId, partialSignatureKeyId, password)
    )

    def fileName(gen: FileGenerator) = gen.fileName + ".gpg"
}

class GpgFileGenerator private (delegate: FileGenerator, f: OutputStream => OutputStream) extends FileGenerator {
    def fileName = GpgFileGenerator.fileName(delegate)

    def write(os: OutputStream) = delegate.write(f(os))
}
