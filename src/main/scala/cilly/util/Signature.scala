/*
 * System.Reflection-like API for acces to .NET assemblies (DLL & EXE)
 */
package cilly.util

/**
 * Signatures
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object Signature {
  /** Marks end of a list. */
  val ELEMENT_TYPE_END: Int = 0x00
  /** void */
  val ELEMENT_TYPE_VOID: Int = 0x01
  /** boolean */
  val ELEMENT_TYPE_BOOLEAN: Int = 0x02
  /** char */
  val ELEMENT_TYPE_CHAR: Int = 0x03
  /** signed byte */
  val ELEMENT_TYPE_I1: Int = 0x04
  /** byte */
  val ELEMENT_TYPE_U1: Int = 0x05
  /** short */
  val ELEMENT_TYPE_I2: Int = 0x06
  /** unsigned short */
  val ELEMENT_TYPE_U2: Int = 0x07
  /** int */
  val ELEMENT_TYPE_I4: Int = 0x08
  /** unsigned int */
  val ELEMENT_TYPE_U4: Int = 0x09
  /** long */
  val ELEMENT_TYPE_I8: Int = 0x0a
  /** unsigned long */
  val ELEMENT_TYPE_U8: Int = 0x0b
  /** float */
  val ELEMENT_TYPE_R4: Int = 0x0c
  /** double */
  val ELEMENT_TYPE_R8: Int = 0x0d
  /** string */
  val ELEMENT_TYPE_STRING: Int = 0x0e
  /** Followed by <type> token. */
  val ELEMENT_TYPE_PTR: Int = 0x0f
  /** Followed by <type> token. */
  val ELEMENT_TYPE_BYREF: Int = 0x10
  /** Followed by <type> token */
  val ELEMENT_TYPE_VALUETYPE: Int = 0x11
  /** Followed by <type> token */
  val ELEMENT_TYPE_CLASS: Int = 0x12
  val ELEMENT_TYPE_VAR: Int = 0x13
  /**
   * <type> <rank> <boundsCount> <bound1> ... <loCount> <lo1> ...
   */
  val ELEMENT_TYPE_ARRAY: Int = 0x14
  val ELEMENT_TYPE_GENERICINST: Int = 0x15
  /** */
  val ELEMENT_TYPE_TYPEDBYREF: Int = 0x16
  /** System.IntPtr */
  val ELEMENT_TYPE_I: Int = 0x18
  /** System.UIntPtr */
  val ELEMENT_TYPE_U: Int = 0x19
  /** Followed by full method signature. */
  val ELEMENT_TYPE_FNPTR: Int = 0x1b
  /** System.Object. */
  val ELEMENT_TYPE_OBJECT: Int = 0x1c
  /** Single-dim array with 0 lower bound. */
  val ELEMENT_TYPE_SZARRAY: Int = 0x1d
  val ELEMENT_TYPE_MVAR: Int = 0x1e
  /** Required modifier : followed by a TypeDef or TypeRef token. */
  val ELEMENT_TYPE_CMOD_REQD: Int = 0x1f
  /** Optional modifier : followed by a TypeDef or TypeRef token. */
  val ELEMENT_TYPE_CMOD_OPT: Int = 0x20
  /** Implemented within the CLI. */
  val ELEMENT_TYPE_INTERNAL: Int = 0x21
  /** Or'd with following element types. */
  val ELEMENT_TYPE_MODIFIER: Int = 0x40
  /** Sentinel for varargs method signature. */
  val ELEMENT_TYPE_SENTINEL: Int = 0x41
  /** Denotes a local variable that points at a pinned object. */
  val ELEMENT_TYPE_PINNED: Int = 0x45
  val HASTHIS: Int = 0x20
  val EXPLICITTHIS: Int = 0x40
  val DEFAULT: Int = 0x00
  val VARARG: Int = 0x05
  val GENERIC: Int = 0x10
  val SENTINEL: Int = 0x41
  val C: Int = 0x01
  val STDCALL: Int = 0x02
  val THISCALL: Int = 0x03
  val FASTCALL: Int = 0x04
  val FIELD: Int = 0x06
  val PROPERTY: Int = 0x08
  val LOCAL_SIG: Int = 0x07
  /** What follows is a string with the full name of the type. */
  val X_ELEMENT_TYPE_TYPE: Int = 0x50
  /** What follows is a string with the full name of the enumeration type */
  val X_ELEMENT_TYPE_ENUM: Int = 0x55
  /** The named argument specifies a field. */
  val X_ELEMENT_KIND_FIELD: Int = 0x53
  /** The named argument specifies a property. */
  val X_ELEMENT_KIND_PROPERTY: Int = 0x54
}
