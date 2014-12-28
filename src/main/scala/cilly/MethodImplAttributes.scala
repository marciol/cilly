/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/**
 * Method implementation attributes
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object MethodImplAttributes {
  /**
   * Specifies flags about code type. 3
   */
  val CodeTypeMask: Short = 0x0003.toShort
  /**
   * Specifies that the method implementation is in MSIL. 0
   */
  val IL: Short = 0x0000.toShort
  /**
   * Specifies that the method implementation is native. 1
   */
  val Native: Short = 0x0001.toShort
  /**
   * This member supports the .NET Framework infrastructure and
   * is not intended to be used directly from your code. 2
   */
  val OPTIL: Short = 0x0002.toShort
  /**
   * Specifies that the method implementation is provided by the runtime. 3
   */
  val Runtime: Short = 0x0003.toShort
  /**
   * Specifies whether the code is managed or unmanaged. 4
   */
  val ManagedMask: Short = 0x0004.toShort
  /**
   * Specifies that the method implementation is managed, otherwise unmanaged.
   */
  val Managed: Short = 0x0000.toShort
  /**
   * Specifies that the method implementation is unmanaged, otherwise managed.
   */
  val Unmanaged: Short = 0x0004.toShort
  /**
   * Specifies that the method cannot be inlined. 8
   */
  val NoInlining: Short = 0x0008.toShort
  /**
   * Specifies that the method is not defined. 16
   */
  val ForwardRef: Short = 0x0010.toShort
  /**
   * Specifies that the method is single-threaded through the body.
   * You can also use the C# lock statement or the Visual Basic
   * Lock function for this purpose. 32
   */
  val Synchronized: Short = 0x0020.toShort
  /**
   * Specifies that the method signature is exported exactly as declared. 128
   */
  val PreserveSig: Short = 0x0080.toShort
  /**
   * Specifies an internal call. 4096
   */
  val InternalCall: Short = 0x1000.toShort
  /**
   * Specifies a range check value. 65535
   */
  val MaxMethodImplVal: Short = 0xffff.toShort

  def toString(implAttr: Int): String = {
    val s: StringBuffer = new StringBuffer
    implAttr & CodeTypeMask match {
      case IL =>
        s.append("cil")
      case Native =>
        s.append("native")
      case Runtime =>
        s.append("runtime")
    }
    implAttr & ManagedMask match {
      case Managed =>
        s.append(" managed")
      case Unmanaged =>
        s.append(" unmanaged")
    }
    if ((implAttr & NoInlining) != 0) s.append(" noinlining")
    if ((implAttr & ForwardRef) != 0) s.append(" forwardref")
    if ((implAttr & Synchronized) != 0) s.append(" synchronized")
    if ((implAttr & InternalCall) != 0) s.append(" internalcall")
    s.toString()
  }
}
