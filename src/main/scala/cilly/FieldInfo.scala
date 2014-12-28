/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import cilly.util.PECustomMod

/**
 * Discovers the attributes of a field and provides access to field metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class FieldInfo(override val name: String, override val declaringType: Type, val attributes: Short, val fieldTypeWithMods: PECustomMod, val value: AnyRef) extends MemberInfo(name, declaringType) with HasCustomModifiers {
  /** Type of the field represented by this FieldInfo object. */
  val fieldType: Type = fieldTypeWithMods.marked
  val cmods: Array[CustomModifier] = fieldTypeWithMods.cmods

  final def memberType: Int = MemberTypes.Field

  final def isStatic: Boolean = (attributes & FieldAttributes.Static) != 0

  final def isInitOnly: Boolean = (attributes & FieldAttributes.InitOnly) != 0

  final def isLiteral: Boolean = (attributes & FieldAttributes.Literal) != 0

  final def isPublic: Boolean = (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.Public

  final def isPrivate: Boolean = (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.Private

  final def isFamily: Boolean = (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.Family

  final def isAssembly: Boolean = (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.Assembly

  final def isFamilyOrAssembly: Boolean = (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.FamORAssem

  final def isFamilyAndAssembly: Boolean = (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.FamANDAssem

  final def isSpecialName: Boolean = (attributes & FieldAttributes.SpecialName) != 0

  final def isPinvokeImpl: Boolean = (attributes & FieldAttributes.PinvokeImpl) != 0

  final def isNotSerialized: Boolean = (attributes & FieldAttributes.NotSerialized) != 0

  private var knownVolatile: Boolean = false
  private var cachedVolatile: Boolean = false

  final def isVolatile: Boolean = {
    if (knownVolatile) return cachedVolatile
    knownVolatile = true
    if (cmods == null) {
      cachedVolatile = false
      return cachedVolatile
    }
    {
      var idx: Int = 0
      while (idx < cmods.length) {
        {
          if (cmods(idx).marker eq CustomModifier.VolatileMarker) {
            cachedVolatile = true
            return cachedVolatile
          }
        }
        ({
          idx += 1; idx - 1
        })
      }
    }
    cachedVolatile = false
    cachedVolatile
  }

  final def getOptionalCustomModifiers: Array[Type] = CustomModifier.helperCustomMods(false, cmods)

  final def getRequiredCustomModifiers: Array[Type] = CustomModifier.helperCustomMods(true, cmods)

  override def toString: String = FieldAttributes.toString(attributes) + " " + fieldType + " " + declaringType.fullName + "::" + name

  /**
    */
  def getValue: AnyRef = value
}