/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/**
 * Specifies type attributes.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object TypeAttributes {
  /** Bitmask used to retrieve visibility information. */
  val VisibilityMask: Int = 0x00000007
  /** Class has no public scope. */
  val NotPublic: Int = 0x00000000
  /** Class has public scope. */
  val Public: Int = 0x00000001
  /** Class is nested with public visibility. */
  val NestedPublic: Int = 0x00000002
  /** Class is nested with private visibility. */
  val NestedPrivate: Int = 0x00000003
  /** Class is nested with family visibility, and is thus accessible
    * only by methods within its own type and any subtypes. */
  val NestedFamily: Int = 0x00000004
  /** Class is nested with assembly visibility, and is thus accessible
    * only by methods within its assembly. */
  val NestedAssembly: Int = 0x00000005
  /** Class is nested with assembly and family visibility, and is thus accessible
    * only by methods lying in the intersection of its family and assembly. */
  val NestedFamANDAssem: Int = 0x00000006
  /** Class is nested with family or assembly visibility, and is thus accessible
    * only by methods lying in the union of its family and assembly. */
  val NestedFamORAssem: Int = 0x00000007
  /** Bitmask used to retrieve class layout information. */
  val LayoutMask: Int = 0x00000018
  /** Class fields are automatically laid out by the CLR. */
  val AutoLayout: Int = 0x00000000
  /** Class fields are laid out sequentially, in the order that the fields
    * were emitted to the metadata. */
  val SequentialLayout: Int = 0x00000008
  /** Class fields are laid out at the specified offsets. */
  val ExplicitLayout: Int = 0x00000010
  /** Bitmask used to retrieve class semantics information. */
  val ClassSemanticsMask: Int = 0x00000020
  /** Type is a class. */
  val Class: Int = 0x00000000
  /** Type is an interface. */
  val Interface: Int = 0x00000020
  /** Class is abstract. */
  val Abstract: Int = 0x00000080
  /** Class is cannot be extended. */
  val Sealed: Int = 0x00000100
  /** Class is special in a way denoted by the name. */
  val SpecialName: Int = 0x00000400
  /** Class/interface is imported from another module. */
  val Import: Int = 0x00001000
  /** Class can be serialized. */
  val Serializable: Int = 0x00002000
  /** Bitmask used to retrieve string information for native interop. */
  val StringFormatMask: Int = 0x00030000
  /** LPTSTR is interpreted as ANSI. */
  val AnsiClass: Int = 0x00000000
  /** LPTSTR is interpreted as UNICODE. */
  val UnicodeClass: Int = 0x00010000
  /** LPTSTR is interpreted automatically. */
  val AutoClass: Int = 0x00020000
  /** Initialize the class before first static field access. */
  val BeforeFieldInit: Int = 0x00100000
  /** CLI provides 'special' behavior, depending upon the name of the type. */
  val RTSpecialName: Int = 0x00000800
  /** Type has security associate with it. */
  val HasSecurity: Int = 0x00040000

  def accessModsToString(attrs: Int): String =
    attrs & VisibilityMask match {
      case NotPublic =>
        "private"
      case Public =>
        "public"
      case NestedPublic =>
        "nested public"
      case NestedPrivate =>
        "nested private"
      case NestedFamily =>
        "nested family"
      case NestedAssembly =>
        "nested assembly"
      case NestedFamANDAssem =>
        "nested famandassem"
      case NestedFamORAssem =>
        "nested famorassem"
      case _ =>
        throw new RuntimeException
    }

  /** Returns a string representation of the given attributes. */
  def toString(attrs: Int): String = {
    val str: StringBuffer = new StringBuffer(accessModsToString(attrs))
    attrs & LayoutMask match {
      case AutoLayout =>
        str.append(" auto") case SequentialLayout =>
        str.append(" sequential")
      case ExplicitLayout =>
        str.append(" explicit")
    }
    attrs & StringFormatMask match {
      case AnsiClass =>
        str.append(" ansi")
      case UnicodeClass =>
        str.append(" unicode")
      case AutoClass =>
        str.append(" autochar")
    }
    if ((attrs & Interface) != 0) str.append(" interface")
    if ((attrs & Abstract) != 0) str.append(" abstract")
    if ((attrs & Sealed) != 0) str.append(" sealed")
    if ((attrs & BeforeFieldInit) != 0) str.append(" beforefieldinit")
    if ((attrs & Serializable) != 0) str.append(" serializable")
    if ((attrs & SpecialName) != 0) str.append(" specialname")
    if ((attrs & RTSpecialName) != 0) str.append(" rtspecialname")
    return str.toString
  }

  /** */
  def isNested(attrs: Int): Boolean = {
    attrs & VisibilityMask match {
      case NestedPublic | NestedPrivate | NestedFamily | NestedAssembly | NestedFamANDAssem | NestedFamORAssem =>
        true
      case _ =>
        false
    }
  }
}
