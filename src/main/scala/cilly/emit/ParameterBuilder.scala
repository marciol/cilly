/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

package cilly.emit

import cilly.Type
import cilly.ConstructorInfo
import cilly.ParameterInfo
import java.io.IOException

/**
 * Creates or associates parameter information.
 * Parameter attributes need to consistent with the method signature.
 * If you specify Out attributes for a parameter, you should ensure that
 * the type of that method parameter is a ByRef type
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class ParameterBuilder(name: String, tpe: Type, attr: Int, pos: Int)
  extends ParameterInfo(name, tpe, attr, pos)
  with ICustomAttributeSetter
  with Visitable {

  //##########################################################################

  /** Sets a custom attribute. */
  def setCustomAttribute(constr: ConstructorInfo, value: Array[Byte]): Unit = {
    addCustomAttribute(constr, value)
  }

  //##########################################################################

  /** The apply method for a visitor */
  @throws(classOf[IOException])
  def apply(v: Visitor): Unit = {
    v.caseParameterBuilder(this)
  }

  //##########################################################################
}
