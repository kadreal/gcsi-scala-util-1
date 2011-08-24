/*
 * ToAsciiConverter.scala
 */

package  com.gaiam.gcsis.util

import org.apache.commons.lang.StringUtils

/**
* Converts a string in utf-8 to contain only ascii printable characters
* so that ecometry can digest them.  If the String is not convertable,
* an exception is thrown (better here than on ecometry injest).
*/
trait Transliterator {
    def tr(str: String) : String
}

trait AsciiTransliterator extends Transliterator {
    override def tr(str: String) : String = {
        if (StringUtils.isAsciiPrintable(str)) {
            return str
        } else {
            throw new CannotTransliterateException("Cannot convert string to be readable by Ecometry:" + str);
        }
    }
}

class CannotTransliterateException(message: String) extends Exception(message)
