/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/**
 * Specifies flags that describe the attributes of a field.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object FieldAttributes {
  /** Specifies the access level of a given field. */
  val FieldAccessMask: Short = 0x0007
  /** Member not refereneceable. */
  val CompilerControlled: Short = 0x0000
  /** Field is accessible only by the parent type. */
  val Private: Short = 0x0001
  /** Field is accessible only by subtypes in this assembly. */
  val FamANDAssem: Short = 0x0002
  /** Field is accessible throughout the assembly. */
  val Assembly: Short = 0x0003
  /** Field is accessible only by type and subtypes. */
  val Family: Short = 0x0004
  /** Field is accessible by subtypes anywhere,
    * as well as throughout this assembly. */
  val FamORAssem: Short = 0x0005
  /** Specifies that the field is accessible by any member
    * for whom this scope is visible. */
  val Public: Short = 0x0006
  /** Field represents the defined type, or else it is per-instance. */
  val Static: Short = 0x0010
  /** Field is initialized only and cannot be written after initialization. */
  val InitOnly: Short = 0x0020
  /** Value is compile-time constant. */
  val Literal: Short = 0x0040
  /** Field does not have to be serialized when the type is remoted. */
  val NotSerialized: Short = 0x0080
  /** Field is special. */
  val SpecialName: Short = 0x0200
  /** Implementation is forwarded through PInvoke */
  val PinvokeImpl: Short = 0x2000
  /** CLI provides 'special' behavior depending upon the name of the field */
  val RTSpecialName: Short = 0x0400
  /** Field has marshalling information. */
  val HasFieldMarshal: Short = 0x1000
  /** Field has a default value. */
  val HasDefault: Short = 0x8000.toShort
  /** Field has a Relative Virtual Address (RVA). The RVA is the location
    * of the method body in the current image, as an address relative
    * to the start of the image file in which it is located. */
  val HasFieldRVA: Short = 0x0100

  def toString(attrs: Short): String = {
    val str: StringBuffer = new StringBuffer
    attrs & FieldAccessMask match {
      case CompilerControlled =>
        str.append("compilercontrolled")
      case Private =>
        str.append("private")
      case FamANDAssem =>
        str.append("famandassem")
      case Assembly =>
        str.append("assembly")
      case Family =>
        str.append("family")
      case FamORAssem =>
        str.append("famorassem")
      case Public =>
        str.append("public")
    }
    if ((attrs & Static) != 0) str.append(" static")
    if ((attrs & InitOnly) != 0) str.append(" initonly")
    if ((attrs & Literal) != 0) str.append(" literal")
    if ((attrs & NotSerialized) != 0) str.append(" notserialized")
    if ((attrs & SpecialName) != 0) str.append(" specialname")
    if ((attrs & PinvokeImpl) != 0) str.append("")
    if ((attrs & RTSpecialName) != 0) str.append(" rtspecialname")
    if ((attrs & HasFieldMarshal) != 0) str.append(" marshal(<native type>)")
    str.toString
  }
}
