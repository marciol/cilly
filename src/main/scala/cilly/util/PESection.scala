/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly.util

import java.io.PrintStream

import cilly.PEFile


object PESection {
  private val buf: Array[Byte] = new Array[Byte](8)
}

/**
 * Describes a section from a PE/COFF file
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final class PESection(val file: PEFile) {
  val sectionStart: Long = file.pos
  file.read(PESection.buf)
  var i: Int = 0

  {
    i = 7
    while ((i >= 0) && (0 == PESection.buf(i))) {
      ({
        i -= 1; i + 1
      })
    }
  }
  val name: String = new String(PESection.buf, 0, i + 1)
  val virtSize = file.readInt
  val virtAddr = file.readInt
  val realSize = file.readInt
  val realAddr = file.readInt
  file.skip(3 * PEFile.INT_SIZE)
  val flags: Int = file.readInt

  def dump(out: PrintStream): Unit = {
    out.println("Section name:    " + name + " (name.length=" + name.length + ")")
    out.println("Virtual Address: 0x" + PEFile.int2hex(virtAddr))
    out.println("Virtual Size:    0x" + PEFile.int2hex(virtSize))
    out.println("Real Address:    0x" + PEFile.int2hex(realAddr))
    out.println("Real Size:       0x" + PEFile.int2hex(realSize))
    out.println("Flags:           0x" + PEFile.int2hex(flags))
  }
}