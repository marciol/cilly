/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

package cilly.emit

import cilly._

import cilly.util.PECustomMod

import java.io.IOException

/**
 * Defines and creates new instances of classes during runtime.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class TypeBuilder(module: Module, attributes: Int, fullName: String, baseType: Type, interfaces: Array[Type], declType: Type)
  extends Type(module, attributes, fullName, baseType, interfaces, declType, 0)
  with ICustomAttributeSetter
  with Visitable {
  import TypeBuilder._

  //##########################################################################
  // public members

  /** 'Bakes' the type. */
  def createType(): Type = {
    fields = fieldBuilders.toArray // (new Array[FieldInfo](fieldBuilders.size())).asInstanceOf[Array[FieldInfo]]
    methods = methodBuilders.toArray // (new Array[MethodInfo](methodBuilders.size())).asInstanceOf[Array[MethodInfo]]
    constructors = constructorBuilders.toArray // (new Array[ConstructorInfo](constructorBuilders.size())).asInstanceOf[Array[ConstructorInfo]]
    nestedTypes = nestedTypeBuilders.toArray // (new Array[Type](nestedTypeBuilders.size())).asInstanceOf[Array[Type]]

    raw = false
    if (declaringType == null)
      Module.asInstanceOf[ModuleBuilder].addType(this)
    return this
  }

  /**
   * Adds a new field to the class, with the given name, attributes and field type. The location has no custom mods.
   */
  def defineField(name: String, fieldType: Type, attrs: Short): FieldBuilder = {
    val fieldTypeWithCustomMods = new PECustomMod(fieldType, null)
    defineField(name, fieldTypeWithCustomMods, attrs)
  }

  /**
   * Adds a new field to the class, with the given name, attributes and (field type , custom mods) combination.
   */
  def defineField(name: String, fieldTypeWithMods: PECustomMod, attrs: Short): FieldBuilder = {
    val field: FieldBuilder = new FieldBuilder(name, this, attrs, fieldTypeWithMods)
    fieldBuilders += field
    return field
  }

  /**
   * Adds a new method to the class, with the given name and
   * method signature.
   */
  def defineMethod(name: String, attrs: Short, returnType: Type, paramTypes: Array[Type]): MethodBuilder = {
    val method = new MethodBuilder(name, this, attrs, returnType, paramTypes)
    val methods = methodBuilders.iterator
    while (methods.hasNext) {
      val m = methods.next().asInstanceOf[MethodInfo]
      if (methodsEqual(m, method)) {
        throw new RuntimeException(s"[${assembly()}] Method has already been defined: $m")
      }
    }
    methodBuilders += method
    return method
  }

  /**
   * Adds a new constructor to the class, with the given attributes
   * and signature.
   */
  def defineConstructor(attrs: Short, callingConvention: Short, paramTypes: Array[Type]): ConstructorBuilder = {
    val constr = new ConstructorBuilder(this, attrs, paramTypes)
    val iter = constructorBuilders.iterator
    while (iter.hasNext) {
      val c = iter.next().asInstanceOf[ConstructorInfo]
      if (constructorsEqual(c, constr)) {
        throw new RuntimeException(s"[${assembly()}] Constructor has already been defined: $c")
      }
    }
    constructorBuilders += constr
    return constr
  }

  /**
   * Defines a nested type given its name.
   */
  def defineNestedType(name: String, attributes: Int, baseType: Type, interfaces: Array[Type]): TypeBuilder = {
    val nested = nestedTypeBuilders.iterator
    while (nested.hasNext) {
      val nt = nested.next
      if (nt.name.equals(name)) {
        val message = s"Nested type $name has already been defined: $nt"
        throw new RuntimeException(message)
      }
    }
    val t = new TypeBuilder(Module, attributes, name, baseType, interfaces, this)
    nestedTypeBuilders += t
    return t
  }

  /** Get the field with the corresponding name. */
  override def getField(name: String): FieldInfo = {
    testRaw(name)
    return super.getField(name)
  }

  /** Get all fields of the current Type. */
  override def getFields(): Array[FieldInfo] = {
    testRaw("<getFields>")
    return super.getFields()
  }

  /**
   * Searches for a public instance constructor whose parameters
   * match the types in the specified array.
   */
  override def getConstructor(params: Array[Type]): ConstructorInfo = {
    testRaw(".ctor" + types2String(params))
    return super.getConstructor(params)
  }

  /**
   * Returns all the public constructors defined for the current Type.
   */
  override def getConstructors(): Array[ConstructorInfo] = {
    testRaw("<getConstructors>")
    return super.getConstructors()
  }

  /**
   * Searches for the specified public method whose parameters
   * match the specified argument types.
   */
  override def getMethod(name: String, params: Array[Type]): MethodInfo = {
    testRaw(name + types2String(params))
    return super.getMethod(name, params)
  }

  /** Returns all the public methods of the current Type. */
  override def getMethods(): Array[MethodInfo] = {
    testRaw("<getMethods>")
    return super.getMethods()
  }

  /** Searches for the nested type with the specified name. */
  override def getNestedType(name: String): Type = {
    testRaw(name)
    super.getNestedType(name)
  }

  /** Returns all the types nested within the current Type. */
  override def getNestedTypes(): Array[Type] = {
    testRaw("<getNestedTypes>")
    super.getNestedTypes()
  }

  /** Returns a Type object that represents a one-dimensional array of the current type */
  def makeArrayType(): Type = {
    Type.mkArray(this, 1)
  }

  /** Sets a custom attribute. */
  def setCustomAttribute(constr: ConstructorInfo, value: Array[Byte]): Unit = {
    addCustomAttribute(constr, value)
  }

  def setPosition(sourceLine: Int, sourceFilename: String): Unit = {
    this.sourceLine = sourceLine
    this.sourceFilename = sourceFilename
  }

  def setSourceFilepath(sourceFilepath: String): Unit = {
    this.sourceFilepath = sourceFilepath
  }

  //##########################################################################
  // protected members

  var sourceLine: Int = _
  var sourceFilename: String = _
  var sourceFilepath: String = _

  var fieldBuilders = scala.collection.mutable.ArrayBuffer.empty[FieldBuilder]
  var methodBuilders = scala.collection.mutable.ArrayBuffer.empty[MethodBuilder]
  var constructorBuilders = scala.collection.mutable.ArrayBuffer.empty[ConstructorBuilder]
  var nestedTypeBuilders = scala.collection.mutable.ArrayBuffer.empty[TypeBuilder]

  // shows if the type is 'raw', i.e. still subject to changes
  private var raw = true

  // throws an exception if the type is 'raw',
  // i.e. not finalized by call to CreateType
  protected def testRaw(member: String): Unit = {
    if (raw)
      throw new RuntimeException(s"Not supported for TypeBuilder before CreateType(): $fullName::$member")
  }

  //##########################################################################
  // public members not part of the Reflection.Emit.TypeBuilder interface.

  /** The apply method for a visitor. */
  @throws(classOf[IOException])
  def apply(v: Visitor): Unit = {
    v.caseTypeBuilder(this)
  }

  //##########################################################################

} // class TypeBuilder

object TypeBuilder {
  def types2String(types: Array[Type]): String = {
    val s = new StringBuffer("(")
    for (i <- 0 until types.length) {
      if (i > 0) s.append(", ")
      s.append(types(i))
    }
    s.append(")")
    return s.toString()
  }

  def methodsEqual(m1: MethodInfo, m2: MethodInfo): Boolean = {
    if (m1.name != m2.name)
      return false
    if (m1.ReturnType != m2.ReturnType)
      return false
    val p1 = m1.getParameters()
    val p2 = m2.getParameters()
    if (p1.length != p2.length)
      return false
    for (i <- 0 until p1.length)
      if (p1(i).parameterType != p2(i).parameterType)
        return false
    return true
  }

  def constructorsEqual(c1: ConstructorInfo, c2: ConstructorInfo): Boolean = {
    if (c1.isStatic != c2.isStatic)
      return false
    val p1 = c1.getParameters()
    val p2 = c2.getParameters()
    if (p1.length != p2.length)
      return false
    for (i <- 0 until p1.length)
      if (p1(i).parameterType != p2(i).parameterType)
        return false
    return true
  }

}
