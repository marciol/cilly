/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly

/**
 * The root class of the Reflection hierarchy.
 *
 * The name of this member.
 *
 * The class that declares this member. Note: if the MemberInfo object is a
 * global member, (that is, it was obtained from Module.getMethods, which
 * returns global methods on a module), then DeclaringType will be a null
 * reference.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
abstract class MemberInfo(val name: String, val declaringType: Type) extends CustomAttributeProvider {

  /**
   * An enumerated value from the MemberTypes class, specifying a constructor,
   * event, field, method, property, type information, all, or custom.
   */
  def memberType: Int
}

object MemberInfo {
  final val EMPTY_ARRAY: Array[MemberInfo] = new Array[MemberInfo](0)
}