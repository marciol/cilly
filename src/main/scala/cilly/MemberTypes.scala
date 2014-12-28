/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/**
 * Marks each type of member that is defined as a derived class of MemberInfo.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object MemberTypes {
  /** Specifies that the member is a constructor,
    * representing a ConstructorInfo member. */
  val Constructor: Int = 0x01
  /** Specifies that the member is an event,
    * representing an EventInfo member. */
  val Event: Int = 0x02
  /** Specifies that the member is a field,
    * representing a FieldInfo member. */
  val Field: Int = 0x04
  /** Specifies that the member is a method,
    * representing a MethodInfo member. */
  val Method: Int = 0x08
  /** Specifies that the member is a property,
    * representing a PropertyInfo member.
    */
  val Property: Int = 0x10
  /** Specifies that the member is a type,
    * representing a TypeInfo member. */
  val TypeInfo: Int = 0x20
  /** Specifies that the member is a custom member type. */
  val Custom: Int = 0x40
  /** Specifies that the member is a nested type,
    * extending MemberInfo. */
  val NestedType: Int = 0x80
  /** Specifies all member types. */
  val All: Int = Constructor | Event | Field | Method | Property | TypeInfo | NestedType

  def toString(memberType: Int): String = {
    if ((memberType & Constructor) != 0) return "Constructor"
    if ((memberType & Event) != 0) return "Event"
    if ((memberType & Field) != 0) return "Field"
    if ((memberType & Method) != 0) return "Method"
    if ((memberType & Property) != 0) return "Property"
    if ((memberType & TypeInfo) != 0) return "TypeInfo"
    if ((memberType & Custom) != 0) return "Custom"
    if ((memberType & NestedType) != 0) return "NestedType"
    return "Unknown MemberType: " + memberType
  }
}
