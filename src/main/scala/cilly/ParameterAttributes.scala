/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly

/**
 * Defines the attributes that may be associated with a parameter.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final case object ParameterAttributes {

  /** Specifies that there is no parameter attribute. */
  final val None: Short = 0x0000

  /** Specifies that the parameter is an input parameter. */
  final val In: Short = 0x0001

  /** Specifies that the parameter is an output parameter. */
  final val Out: Short = 0x0002

  /** Specifies that the parameter is a locale identifier. */
  final val Lcid: Short = 0x0004

  /** Specifies that the parameter is a return value. */
  final val Retval: Short = 0x0008

  /**
   * Specifies that the parameter is optional.
   *  Attention: In the specification the value is 0x0004 but
   *  in mscorlib.dll that it Lcid and Optional is 0x0010
   */
  final val Optional: Short = 0x0010

  /** Specifies that the parameter has a default value. */
  final val HasDefault: Short = 0x1000

  /** Specifies that the parameter has field marshaling information. */
  final val HasFieldMarshal: Short = 0x2000

  /** Reserved. */
  final val Reserved3: Short = 0x4000

  /** Reserved. */
  final val Reserved4: Short = 0x8000.toShort

  /** Specifies that the parameter is reserved. */
  final val ReservedMask: Short = 0xf000.toShort

  /** Reserved: shall be zero in all conforming implementations. */
  final val Unused: Short = 0xcfe0.toShort

  final def toString(attrs: Int) = {
    val s = new StringBuffer()
    if ((attrs & In) != 0) s.append("in ")
    if ((attrs & Out) != 0) s.append("out ")
    if ((attrs & Optional) != 0) s.append("opt ")
    if ((attrs & HasDefault) != 0) s.append("default(???) ")
    if ((attrs & HasFieldMarshal) != 0) s.append("marshal(???) ")
    s.toString
  }
}
