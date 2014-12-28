/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.util.{HashMap, Map}

/**
 * Defines and represents a module. Get an instance of ModuleBuilder by calling
 * DefineDynamicModule A module is a portable executable file of type .dll or
 * .exe consisting of one or more classes and interfaces. There may be multiple
 * namespaces contained in a single module, and a namespace may span multiple
 * modules. One or more modules deployed as a unit compose an assembly.
 *
 * @param name               String representing the name of the module with the path removed.
 * @param fullyQualifiedName String representing the fully qualified name and path to this module.
 * @param scopeName          String representing the name of the module.
 * @param assembly           The Assembly the Module belongs to.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
abstract class Module(val name: String, val fullyQualifiedName: String, val scopeName: String, val assembly: Assembly) extends CustomAttributeProvider {
  /** Returns the specified class, performing a case-sensitive search. */
  def getType(name: String): Type = {
    doInitTypes()
    typesMap.get(name)
  }

  /**
   * @return all the classes defined within this module.
   */
  def getTypes: Array[Type] = {
    doInitTypes()
    types.clone
  }

  /**
   * @return the global field with the specified name.
   */
  def getField(name: String): FieldInfo =
    fields.find(field => field.name == name).getOrElse(null)

  /**
   * @return an array of the global fields of the module
   */
  def getFields: Array[FieldInfo] = fields.clone

  /**
   * @return - the global method with the specified name
   */
  def getMethod(name: String): MethodInfo =
    methods.find(method => method.name == name).getOrElse(null)

  /**
   * @return - an array of all the global methods defined in this modules.
   */
  def getMethods: Array[MethodInfo] = methods.clone

  override def toString: String = name

  protected final val typesMap: Map[String, Type] = new HashMap[String, Type]
  protected var types: Array[Type] = null
  protected var fields: Array[FieldInfo] = Array.empty
  protected var methods: Array[MethodInfo] = Array.empty

  protected def addType(typ: Type): Type = {
    addType(typ.fullName, typ)
    assembly.addType(typ)
    typ
  }

  protected def addType(name: String, typ: Type): Type = {
    assert(typ != null)
    typesMap.put(name, typ)
    typ
  }

  private var initTypes: Boolean = true

  protected final def doInitTypes(): Unit = {
    if (initTypes) {
      loadTypes()
      initTypes = false
    }
  }

  protected def loadTypes(): Unit = {}

  private var initGlobals: Boolean = true

  protected final def doInitGlobals(): Unit = {
    if (initGlobals) {
      loadGlobals
      initGlobals = false
    }
  }

  protected def loadGlobals(): Unit = {}
}
