/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/**
 * Discovers the attributes of a property and provides access to property
 * metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class PropertyInfo protected(override val name: String, override val declaringType: Type, val attributes: Short, val propertyType: Type, val getter: MethodInfo, val setter: MethodInfo) extends MemberInfo(name, declaringType) {
  final def memberType: Int = MemberTypes.Property

  final def CanRead: Boolean = getter != null
  final def CanWrite: Boolean = setter != null

  /**
   * Returns an array of the public get and set accessors for this property.
   */
  def publicAccessors: Array[MethodInfo] = accessors(false)

  /**
   * Returns an array of the public or non-public <b>get</b> and <b>set</b>
   * accessors for this property.
   */
  def accessors(nonPublic: Boolean): Array[MethodInfo] = {
    val getter: MethodInfo = getMethod(nonPublic)
    val setter: MethodInfo = setMethod(nonPublic)
    if (getter == null)
      if (setter == null)
        Array.empty[MethodInfo]
      else
        Array[MethodInfo](setter)
    else if (setter == null)
      Array[MethodInfo](getter)
    else
      Array[MethodInfo](getter, setter)
  }

  /**
   * Returns the public <b>get</b> accessor for this property.
   */
  def publicGetMethod: MethodInfo = getMethod(false)

  /**
   * Returns the public or non-public <b>get</b> accessor for this property.
   */
  def getMethod(nonPublic: Boolean): MethodInfo =
    if (nonPublic) getter else if (getter == null || getter.isPublic) getter else null

  /**
   * Returns the public <b>set</b> accessor for this property.
   */
  def publicSetMethod: MethodInfo = setMethod(false)

  /**
   * Returns the public or non-public <b>set</b> accessor for this property.
   */
  def setMethod(nonPublic: Boolean): MethodInfo =
    if (nonPublic) setter else if (setter == null || setter.isPublic) setter else null

  override def toString: String = {
    val m: MethodInfo = if (getter != null) getter else setter
    MethodAttributes.accessFlagsToString((if (getter != null) getter else setter).attributes) + " " + PropertyAttributes.toString(attributes) + declaringType + "::" + name
  }
}