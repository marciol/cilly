/*
 * System.Reflection-like API for acces to .NET assemblies (DLL & EXE)
 */
package cilly.util

import java.io.{UnsupportedEncodingException, PrintStream}
import java.nio.ByteBuffer

import cilly.PEFile
import cilly.util.Signature._

/**
 * Implements support for CLI streams within a PE file.
 *
 * @param file The PEFile to which this stream belongs
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final class PEStream(val file: PEFile) {
  /** The offset of the stream from the beginning of the file. */
  val offset: Int = file.fromRVA(file.rvaMetadata + file.readInt)
  /** The size of the stream in bytes; shall be multiple of 4. */
  val size: Int = file.readInt
  val buffer: ByteBuffer = file.getBuffer(offset, size)
  var i: Int = 0
  val _buf: Array[Byte] = new Array[Byte](16)
  do {
    _buf(i) = file.readByte.toByte
    i += 1
  } while (0 != _buf(i - 1))
  /** The name of the stream. */
  val name: String = new String(_buf, 0, i - 1)
  file.align(PEFile.INT_SIZE, file.posMetadata)

  /** Move to the specified position in the stream. */
  private def seek(pos: Int): Unit = {
    try {
      buffer.position(pos)
    } catch {
      case e: IllegalArgumentException => {
        System.err.println("\nSeek failed in file " + file + " for position " + pos + " of stream " + name + " (" + buffer + ")")
        throw e
      }
    }
  }

  /** Return a string from the specified position in the stream. */
  def getString(pos: Int): String = {
    seek(pos)
    buffer.mark
    var i: Int = 0

    {
      i = 0
      while (getByte != 0) {
        ({
          i += 1; i - 1
        })
      }
    }
    val buf: Array[Byte] = new Array[Byte](i)
    buffer.reset
    buffer.get(buf)
    try {
      return new String(buf, "UTF-8")
    }
    catch {
      case e: UnsupportedEncodingException => {
        throw new RuntimeException(e)
      }
    }
  }

  /** Read a byte from the stream. */
  def getByte: Int = {
    return (buffer.get + 0x0100) & 0xff
  }

  /** Return the GUID at the given position in the stream. */
  def getGUID(pos: Int): Array[Byte] = {
    seek(pos)
    val buf: Array[Byte] = new Array[Byte](32)
    try {
      buffer.get(buf)
    }
    catch {
      case e: Exception => {
        System.err.println()
        System.err.println("PEStream.getBlob(): Exception for pos = " + pos + " and buf.length = " + buf.length)
        System.err.println("\tbuffer = " + buffer)
        e.printStackTrace()
        throw new RuntimeException
      }
    }
    return buf
  }

  def readLength: Int = {
    var length: Int = getByte
    if ((length & 0x80) != 0) {
      length = ((length & 0x7f) << 8) | getByte
      if ((length & 0x4000) != 0) length = ((length & 0x3fff) << 16) | (getByte << 8) | getByte
    }
    return length
  }

  /** Return a blob from the specified position in the stream. */
  def getBlob(pos: Int): Array[Byte] = {
    seek(pos)
    val length: Int = readLength
    val buf: Array[Byte] = new Array[Byte](length)
    buffer.get(buf)
    return buf
  }

  /** */
  def getSignature(pos: Int): PEFile#Sig = {
    seek(pos)
    return file.newSignature(buffer)
  }

  /**
    */
  def getConstant(typ: Int, pos: Int): AnyRef = {
    seek(pos)
    val length: Int = readLength
    typ match {
      case ELEMENT_TYPE_BOOLEAN =>
        assert(length == 1)
        if (buffer.get == 0) java.lang.Boolean.FALSE else java.lang.Boolean.TRUE
      case ELEMENT_TYPE_CHAR =>
        assert(length == 2, "length == " + length)
        new java.lang.Character(buffer.getChar)
      case ELEMENT_TYPE_I1 | ELEMENT_TYPE_U1 =>
        assert(length == 1)
        new java.lang.Byte(buffer.get)
      case ELEMENT_TYPE_I2 | ELEMENT_TYPE_U2 =>
        assert(length == 2)
        new java.lang.Short(buffer.getShort)
      case ELEMENT_TYPE_I4 | ELEMENT_TYPE_U4 =>
        assert(length == 4)
        new java.lang.Integer(buffer.getInt)
      case ELEMENT_TYPE_I8 | ELEMENT_TYPE_U8 =>
        assert(length == 8)
        new java.lang.Long(buffer.getLong)
      case ELEMENT_TYPE_R4 =>
        assert(length == 4)
        new java.lang.Float(buffer.getFloat)
      case ELEMENT_TYPE_R8 =>
        assert(length == 8)
        new java.lang.Double(buffer.getDouble)
      case ELEMENT_TYPE_STRING =>
        try {
          new String(getBlob(pos), "UTF-16LE")
        } catch {
          case e: UnsupportedEncodingException => {
            throw new RuntimeException(e)
          }
        }
      case _ =>
        throw new RuntimeException("Illegal constant type: " + typ)
    }
  }

  def dump(out: PrintStream): Unit = {
    out.println("Stream name:   " + name + " (length " + name.length + " characters)")
    out.println("Stream offset: 0x" + PEFile.int2hex(offset))
    out.println("Stream size:   0x" + PEFile.int2hex(size))
  }
}