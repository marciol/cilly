/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

package cilly.emit

import cilly.FieldInfo
import cilly.Type
import cilly.FieldAttributes
import cilly.ConstructorInfo

import cilly.util.PECustomMod

import java.io.IOException

/**
 * Discovers the attributes of a field and provides access to field metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class FieldBuilder(name: String, declType: Type, attrs: Int, fieldTypeWithMods: PECustomMod)
  extends FieldInfo(name, declType, attrs, fieldTypeWithMods, null)
  with ICustomAttributeSetter
  with Visitable {

  //##########################################################################
  // public interface

  /** Sets a custom attribute. */
  def setCustomAttribute(constr: ConstructorInfo, value: Array[Byte]): Unit = {
    addCustomAttribute(constr, value)
  }

  //##########################################################################

  /** the apply method for a visitor */
  @throws(classOf[IOException])
  def apply(v: Visitor): Unit = {
    v.caseFieldBuilder(this)
  }

  //##########################################################################

  protected var defaultValue: Object = _

  /** Sets the default value of this field. */
  def setConstant(defaultValue: Object): Unit = {
    this.defaultValue = defaultValue
  }

  /** Specifies the field layout. */
  def setOffset(iOffset: Int): Unit = {
    //this.fieldOffset = FieldAttributes.Offset.Value(iOffset)
  }

  //##########################################################################
}
