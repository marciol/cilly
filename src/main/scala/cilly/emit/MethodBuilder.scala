/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

package cilly.emit

import cilly.MethodInfo
import cilly.ParameterInfo
import cilly.Type
import cilly.ConstructorInfo
import java.io.IOException

/**
 * Defines and represents a method of a dynamic class.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class MethodBuilder(name: String, declType: Type, attrs: Short, returnType: Type, paramTypes: Array[Type])
  extends MethodInfo(name, declType, attrs, returnType, paramTypes)
  with ICustomAttributeSetter
  with Visitable {

  //##########################################################################
  // public interface

  /**
   * Defines a parameter of this method. TODO: Parameters are indexed staring
   *  from number 1 for the first parameter
   */
  def defineParameter(pos: Int, attr: Int, name: String): ParameterBuilder = {
    val param = new ParameterBuilder(name, params(pos).parameterType, attr, pos)
    params(pos) = param
    return param
  }

  /** Returns an ILGenerator for this method. */
  def getILGenerator: ILGenerator = {
    if (ilGenerator == null)
      throw new RuntimeException
    ("No code generator available for this method: " + this)
    return ilGenerator
  }

  /** Sets a custom attribute. */
  def setCustomAttribute(constr: ConstructorInfo, value: Array[Byte]): Unit = {
    addCustomAttribute(constr, value)
  }

  //##########################################################################

  /** The apply method for a visitor. */
  @throws(classOf[IOException])
  def apply(v: Visitor): Unit = {
    v.caseMethodBuilder(this)
  }

  //##########################################################################

  // the Intermediate Language Generator
  // it contains the method's body
  protected final val ilGenerator: ILGenerator =
    if (declaringType == null // global method
      || !declaringType.isInterface)
      new ILGenerator(this)
    else null

  //##########################################################################
}
