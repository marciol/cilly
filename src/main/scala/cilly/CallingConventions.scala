/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly

/**
 * Calling conventions
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final case object CallingConventions {

  /**
   * Specifies the default calling convention as determined by the
   * common language runtime.
   */
  final val Standard: Short = 0x0001

  /**
   * Specifies the calling convention for methods with variable arguments.
   */
  final val VarArgs: Short = 0x0002

  /**
   * Specifies that either the Standard or the VarArgs calling
   * convention may be used.
   */
  final val Any: Short = (Standard | VarArgs).toShort

  /**
   * Specifies an instance or virtual method (not a static method).
   * At run-time, the called method is passed a pointer to the target
   * object as its first argument (the this pointer). The signature
   * stored in metadata does not include the type of this first argument,
   * because the method is known and its owner class can be discovered
   * from metadata.
   */
  final val HasThis: Short = 0x0020

  /**
   * Specifies that the signature is a function-pointer signature,
   * representing a call to an instance or virtual method (not a static
   * method). If ExplicitThis is set, HasThis must also be set. The first
   * argument passed to the called method is still a this pointer, but the
   * type of the first argument is now unknown. Therefore, a token that
   * describes the type (or class) of the this pointer is explicitly stored
   * into its metadata signature.
   */
  final val ExplicitThis: Short = 0x0040

  //########################################################################

  def toString(callConv: Int) = {
    val s = new StringBuffer()

    if ((callConv & HasThis) != 0) {
      s.append("instance")
      if ((callConv & ExplicitThis) != 0)
        s.append(" explicit")
    }
    s.toString
  }
}
