/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/** Specifies flags for method attributes.
  *
  * @author Nikolay Mihaylov
  * @version 1.0
  */
object MethodAttributes {
  /** Bitmask used to retrieve accessibility information. */
  val MemberAccessMask: Short = 0x0007
  /** Indicates that the member cannot be referenced. */
  val PrivateScope: Short = 0x0000
  /** Method is accessible only by the current class. */
  val Private: Short = 0x0001
  /** Method is accessible to members of this type
    * and its derived types that are in this assembly only. */
  val FamANDAssem: Short = 0x0002
  /** Method is accessible to any class of this assembly. */
  val Assembly: Short = 0x0003
  /** Method is accessible only to members of this class
    * and its derived classes. */
  val Family: Short = 0x0004
  /** Method is accessible to derived classes anywhere,
    * as well as to any class in the assembly. */
  val FamORAssem: Short = 0x0005
  /** Method is accessible to any object for which this object is in scope. */
  val Public: Short = 0x0006
  /** Method is defined on the type; otherwise, it is defined per instance. */
  val Static: Short = 0x0010
  /** Method cannot be overridden. */
  val Final: Short = 0x0020
  /** Method is virtual. */
  val Virtual: Short = 0x0040
  /** Method hides by name and signature; otherwise, by name only. */
  val HideBySig: Short = 0x0080
  /** Bitmask used to retrieve vtable attributes. */
  val VtableLayoutMask: Short = 0x0100
  /** Method reuses existing slot in the vtable. */
  val ReuseSlot: Short = 0x0000
  /** Method always gets a new slot in the vtable. */
  val NewSlot: Short = 0x0100
  /** Method does not provide implementation. */
  val Abstract: Short = 0x0400
  /** Method is special. */
  val SpecialName: Short = 0x0800
  /** Method implementation is forwarded through PInvoke. */
  val PInvokeImpl: Short = 0x2000
  /** Reserved: shall be zero for conforming implementations.
    * Managed method is exported by thunk to unmanaged code. */
  val UnmanagedExport: Short = 0x0008
  /** CLI provides special behavior, depending on the name of the method. */
  val RTSpecialName: Short = 0x1000
  /** Method has security associated with it.
    * Reserved flag for runtime use only.
    */
  val HasSecurity: Short = 0x00000040
  /**
   * Indicates that the method calls another method containing security code.
   * Reserved flag for runtime use only.
   */
  val RequireSecObject: Short = 0x00004000
  /** Indicates a reserved flag for runtime use only. */
  val ReservedMask: Short = 0x0000

  def toString(attrs: Short): String = {
    val str: StringBuffer = new StringBuffer(accessFlagsToString(attrs))
    if ((attrs & Static) != 0) str.append(" static")
    if ((attrs & Final) != 0) str.append(" final")
    if ((attrs & Virtual) != 0) str.append(" virtual")
    if ((attrs & Abstract) != 0) str.append(" abstract")
    if ((attrs & HideBySig) != 0) str.append(" hidebysig")
    if ((attrs & NewSlot) != 0) str.append(" newslot")
    if ((attrs & SpecialName) != 0) str.append(" specialname")
    if ((attrs & PInvokeImpl) != 0) str.append(" pinvokeimpl(?!?)")
    if ((attrs & RTSpecialName) != 0) str.append(" rtspecialname")
    str.toString()
  }

  def accessFlagsToString(attrs: Short): String = {
    attrs & MemberAccessMask match {
      case PrivateScope =>
        return "compilercontrolled"
      case Private =>
        return "private"
      case FamANDAssem =>
        return "famandassem"
      case Assembly =>
        return "assembly"
      case Family =>
        return "family"
      case FamORAssem =>
        return "famorassem"
      case Public =>
        return "public"
      case _ =>
        return "xxx"
    }
  }
}
