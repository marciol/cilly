/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

package cilly.emit

import cilly._
import java.io.IOException

/**
 * Defines and represents a module. Get an instance of ModuleBuilder
 * by calling DefineDynamicModule
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class ModuleBuilder(name: String, fullname: String, scopeName: String, assembly: Assembly)
  extends Module(name, fullname, scopeName, assembly)
  with ICustomAttributeSetter
  with Visitable {

  //##########################################################################
  // public interface

  /**
   * Complete the global function definitions for this dynamic module.
   * This method should be called when the user is done with defining
   * all of the global functions within this dynamic module. After calling
   * this function, no more new global functions or new global data are
   * allowed.
   */
  def CreateGlobalFunctions(): Unit = {
    if (globalsCreated)
      throw new RuntimeException("Global functions are already created")
    this.fields = fieldBuilders.toArray // (fields).asInstanceOf[Array[FieldInfo]]
    this.methods = methodBuilders.toArray //  (methods).asInstanceOf[Array[MethodInfo]]
    globalsCreated = true
  }

  /**
   * Constructs a TypeBuilder for a type with the specified name
   */
  def DefineType(typeName: String): TypeBuilder = {
    return defineType(typeName, 0, null, Type.EmptyTypes)
  }

  /**
   * Constructs a TypeBuilder for a type with the specified name
   * and specified attributes
   */
  def DefineType(typeName: String, attributes: Int): TypeBuilder = {
    return defineType(typeName, attributes, null, Type.EmptyTypes)
  }

  /**
   * Constructs a TypeBuilder given type name, its attributes,
   * and the type that the defined type extends.
   */
  def DefineType(typeName: String, attributes: Int,
    baseType: Type): TypeBuilder = {
    return defineType(typeName, attributes, baseType, Type.EmptyTypes)
  }

  /**
   * Constructs a TypeBuilder given the Full specification of a type,
   * Given the type name, attributes, the type that the defined type
   * extends, and the interfaces that the defined type implements.
   */
  def defineType(typeName: String, attributes: Int, baseType: Type, interfaces: Array[Type]): TypeBuilder = {
    val t: Type = getType(typeName) // Module.GetType(String)
    if (t != null)
      throw new RuntimeException(s"Type [$assembly] $typeName already exists!")
    val `type` =
      new TypeBuilder(this, attributes, typeName, baseType, interfaces, null)
    addType(`type`)
    return `type`
  }

  /**
   * Defines a global method given its name, attributes, return type, and
   * parameter types.
   */
  def defineGlobalMethod(name: String, attributes: Short,
    returnType: Type, paramTypes: Array[Type]): MethodBuilder =
    {
      val method =
        new MethodBuilder(name, null, attributes, returnType, paramTypes)
      methodBuilders += method
      return method
    }

  override def getTypes: Array[Type] = {
    val res = scala.collection.mutable.ArrayBuffer.empty[Type]
    val iter = typesMap.values().iterator
    while (iter.hasNext) {
      res += iter.next
    }
    return res.toArray
  }

  /** Sets a custom attribute. */
  def setCustomAttribute(constr: ConstructorInfo, value: Array[Byte]): Unit = {
    addCustomAttribute(constr, value)
  }

  //##########################################################################
  // internal members

  var globalsCreated = false
  protected var fieldBuilders = scala.collection.mutable.ArrayBuffer.empty[FieldInfo]
  protected var methodBuilders = scala.collection.mutable.ArrayBuffer.empty[MethodInfo]

  override def addType(t: Type): Type = {
    return super.addType(t)
  }

  //##########################################################################

  /**
   * the apply method for a visitor
   */
  @throws(classOf[IOException])
  def apply(v: Visitor): Unit = {
    v.caseModuleBuilder(this)
  }

  //##########################################################################
}
