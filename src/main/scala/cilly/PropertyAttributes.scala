/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/**
 * Attributes applcicable to properties
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object PropertyAttributes {
  /** Specifies that the property is special, with the name describing
    * how the property is special.
    */
  val SpecialName: Short = 0x0200
  /** Specifies that the metadata internal APIs check the name encoding.
    */
  val RTSpecialName: Short = 0x0400
  /** Specifies that the property has a default value.
    */
  val HasDefault: Short = 0x1000

  def toString(attrs: Short): String = {
    val str: StringBuffer = new StringBuffer
    if ((attrs & SpecialName) != 0) str.append("specialname ")
    if ((attrs & RTSpecialName) != 0) str.append("rtspecialname ")
    str.toString()
  }
}