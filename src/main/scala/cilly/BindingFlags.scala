/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/**
 * Specifies flags that control binding and the way in which
 * the search for members and types is conducted by reflection.
 *
 * Note: You must specify Instance or Static along with Public or NonPublic
 * or no members will be returned.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object BindingFlags {
  /**
   * Specifies no binding flag.
   */
  val Default: Int = 0x0000
  /**
   * Specifies that the case of the member name should not be considered
   * when binding.
   */
  val IgnoreCase: Int = 0x0001
  /**
   * Specifies that only members declared at the level of the supplied type's
   * hierarchy should be considered. Inherited members are not considered.
   */
  val DeclaredOnly: Int = 0x0002
  /**
   * Specifies that instance members are to be included in the search.
   */
  val Instance: Int = 0x0004
  /**
   * Specifies that static members are to be included in the search.
   */
  val Static: Int = 0x0008
  /**
   * Specifies that public members are to be included in the search.
   */
  val Public: Int = 0x0010
  /**
   * Specifies that non-public members are to be included in the search.
   */
  val NonPublic: Int = 0x0020
  /**
   * Specifies that static members up the hierarchy should be returned.
   * Static members include fields, methods, events, and properties.
   * Nested types are not returned.
   */
  val FlattenHierarchy: Int = 0x0040
  /**
   * Specifies that a method is to be invoked. This may not be a constructor
   * or a type initializer.
   */
  val InvokeMethod: Int = 0x0100
  /**
   * Specifies that Reflection should create an instance of
   * the specified type. Calls the constructor that matches
   * the given arguments. The supplied member name is ignored.
   * If the type of lookup is not specified, (Instance | Public)
   * will apply. It is not possible to call a type initializer.
   */
  val CreateInstance: Int = 0x0200
  /**
   * Specifies that the value of the specified field should be returned.
   */
  val GetField: Int = 0x0400
  /**
   * Specifies that the value of the specified field should be set.
   */
  val SetField: Int = 0x0800
  /**
   * Specifies that the value of the specified property should be returned.
   */
  val GetProperty: Int = 0x1000
  /**
   * Specifies that the value of the specified property should be set.
   * For COM properties, specifying this binding flag is equivalent to
   * specifying PutDispProperty and PutRefDispProperty.
   */
  val SetProperty: Int = 0x2000
  /**
   * Specifies that the PROPPUT member on a COM object should be invoked.
   * PROPPUT specifies a property-setting function that uses a value.
   * Use PutDispProperty if a property has both PROPPUT and PROPPUTREF
   * and you need to distinguish which one is called.
   */
  val PutDispProperty: Int = 0x4000
  /**
   * Specifies that the PROPPUTREF member on a COM object should be invoked.
   * PROPPUTREF specifies a property-setting function that uses a reference
   * instead of a value. Use PutRefDispProperty if a property has both
   * PROPPUT and PROPPUTREF and you need to distinguish which one is called.
   */
  val PutRefDispProperty: Int = 0x8000
  /**
   * Specifies that types of the supplied arguments must exactly match
   * the types of the corresponding formal parameters. Reflection
   * throws an exception if the caller supplies a non-null Binder object,
   * since that implies that the caller is supplying BindToXXX
   * implementations that will pick the appropriate method.
   * Reflection models the accessibility rules of the common type system.
   * For example, if the caller is in the same assembly, the caller
   * does not need special permissions for internal members. Otherwise,
   * the caller needs ReflectionPermission. This is consistent with
   * lookup of members that are protected, private, and so on.
   * The general principle is that ChangeType should perform only
   * widening coercions, which never lose data. An example of a
   * widening coercion is coercing a value that is a 32-bit signed integer
   * to a value that is a 64-bit signed integer. This is distinguished
   * from a narrowing coercion, which may lose data. An example of
   * a narrowing coercion is coercing a 64-bit signed integer to
   * a 32-bit signed integer.
   * The default binder ignores this flag, while custom binders can
   * implement the semantics of this flag.
   */
  val ExactBinding: Int = 0x10000
  /**
   * Used in COM interop to specify that the return value of the member
   * can be ignored.
   */
  val IgnoreReturn: Int = 0x100000
  /**
   * Returns the set of members whose parameter count matches the number
   * of supplied arguments. This binding flag is used for methods with
   * parameters that have default values and methods with variable arguments
   * (varargs). This flag should only be used with Type.InvokeMember.
   * Parameters with default values are used only in calls where trailing
   * arguments are omitted. They must be the last arguments.
   */
  val OptionalParamBinding: Int = 0x40000
  /**
   * Not implemented.
   */
  val SuppressChangeType: Int = 0x20000
}
