/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.util.{ArrayList, Arrays, HashMap, Iterator, List, Map}

/**
 * Represents type declarations: class types, interface types, array types,
 * value types, and enumeration types.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object Type {
  /** Empty array of type Type. */
  val EmptyTypes: Array[Type] = new Array[Type](0)
  /** Separates names in the namespace of the Type. */
  val Delimiter: Char = '.'
  private val types: Map[String, Type] = new HashMap[String, Type]

  def getTypeInternal(name: String): Type = {
    return types.get(name)
  }

  def addType(t: Type): Type = {
    assert((!(t.isInstanceOf[TMVarUsage])))
    assert((!(t.isInstanceOf[ConstructedType])))
    val oldType: Type = types.put(t.fullName, t)
    return t
  }

  /**
   * Non-defining reference to either a TVar or an MVar. An instance of
   * GenericParamAndConstraints represents a TVar or an MVar definition.
   */
  final class TMVarUsage(val number: Int, val isTVar: Boolean) extends Type(null, 0, (if (isTVar) "!" else "!!") + number, null, null, null, AuxAttr.None, null) {

    override def toString: String = (if (isTVar) "!" else "!!") + number

    final override def isTMVarUsage: Boolean = true

    override def equals(o: Any): Boolean = {
      if (this eq o.asInstanceOf[AnyRef]) return true
      if (o == null || (getClass ne o.getClass)) return false
      val that: TMVarUsage = o.asInstanceOf[TMVarUsage]
      if (number != that.number) return false
      if (isTVar != that.isTVar) return false
      true
    }

    override def hashCode: Int = {
      var result: Int = number
      result = 31 * result + (if (isTVar) 1 else 0)
      result
    }
  }

  object AuxAttr {
    val None: Int = 0x0000
    val Array: Int = 0x0001
    val ByRef: Int = 0x0002
    val Pointer: Int = 0x0008
    val Primitive: Int = 0x0010
  }

  /** */
  def mkArray(elemType: Type, rank: Int): Type = {
    val arrSig: StringBuffer = new StringBuffer("[")

    {
      var i: Int = 0
      while (i < rank) {
        {
          if (i > 0) arrSig.append(',')
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
    arrSig.append(']')
    var array: Type = getTypeInternal(elemType.fullName + arrSig)
    if (array != null) return array
    array = new PrimitiveType(elemType.module, elemType.attributes | TypeAttributes.Sealed | TypeAttributes.Serializable, elemType.fullName + arrSig, ARRAY, EmptyTypes, null, AuxAttr.Array, elemType)
    addType(array)
  }

  /** */
  def mkPtr(elemType: Type): Type = {
    val name: String = elemType.fullName + "*"
    var typ: Type = getTypeInternal(name)
    if (typ != null) return typ
    typ = new PrimitiveType(elemType.module, elemType.attributes, name, null, EmptyTypes, null, AuxAttr.Pointer, elemType)
    addType(typ)
  }

  /** */
  def mkByRef(elemType: Type): Type = {
    val name: String = elemType.fullName + "&"
    var typ: Type = getTypeInternal(name)
    if (typ != null) return typ
    typ = new PrimitiveType(elemType.module, elemType.attributes, name, null, EmptyTypes, null, AuxAttr.ByRef, elemType)
    addType(typ)
  }

  /**
   * Return the type with the specified signature parameters. For example, the
   * fully qualified name for a class might look like this:
   * TopNamespace.SubNameSpace.ContainingClass+NestedClass,MyAssembly
   */
  def getType(fullName: String): Type = {
    var `type`: Type = getTypeInternal(fullName)
    if (`type` != null) return `type`
    var i: Int = fullName.lastIndexOf('[')
    val j: Int = fullName.lastIndexOf(']')
    if (i >= 0) if (j > i && j == (fullName.length - 1)) {
      val elementTypeName: String = fullName.substring(0, i)
      val elementType: Type = getType(elementTypeName)
      if (elementType == null) throw new RuntimeException("Unknown element type '" + elementTypeName + "' for the array type: " + fullName)
      val rank: Int = j - i

      {
        var k: Int = i + 1
        while (k < j) {
          {
            if (fullName.charAt(k) != ',') throw new RuntimeException("Malformed type name: " + fullName)
          }
          ({
            k += 1;
            k - 1
          })
        }
      }
      return mkArray(elementType, rank)
    }
    else throw new RuntimeException("Malformed type name: " + fullName)
    if (fullName.charAt(fullName.length - 1) == '*') return addType(mkPtr(getType(fullName.substring(0, fullName.length - 1))))
    i = fullName.lastIndexOf('+')
    if (i > 0) {
      if (i == 0 || i == (fullName.length - 1)) throw new RuntimeException("malformedTypeName")
      val enclosing: Type = getType(fullName.substring(0, i))
      return if (enclosing == null) null else enclosing.getNestedType(fullName.substring(i + 1))
    }
    val assems: Iterator[Assembly] = cilly.Assembly.assemblies.values.iterator
    while (`type` == null && assems.hasNext) {
      val assem: Assembly = (assems.next)
      `type` = assem.getType(fullName)
    }
    val type2: Type = getTypeInternal(fullName)
    if (`type` eq type2) return `type`
    if (`type` == null) null else addType(`type`)
  }

  /**
    */
  protected def findMethod(methods: Array[MethodInfo], name: String, paramTypes: Array[Type], retType: Type): MethodInfo = {
    var i: Int = 0
    while (i < methods.length) {
      if ((name == methods(i).name) && equalParameters(methods(i).getParameters, paramTypes) && (retType == null || (methods(i).returnType eq retType))) return methods(i)
      i += 1
    }
    null
  }

  /**
    */
  protected def equalParameters(params: Array[ParameterInfo], paramTypes: Array[Type]): Boolean = {
    if (params.length != paramTypes.length) return false

    {
      var i: Int = 0
      while (i < params.length) {
        {
          if (params(i).parameterType ne paramTypes(i)) return false
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
    return true
  }

  private def formatType(t: Type): String = {
    if (t == null) return "<null>"
    var cname: String = t.getClass.getName
    val k: Int = cname.lastIndexOf(".")
    if (k >= 0) cname = cname.substring(k + 1)
    return "[" + t.assembly.getName + "]" + t + "(" + cname + "#" + Integer.toHexString(t.hashCode) + ")"
  }

  private def dumpType(t: Type): String = {
    val str: StringBuffer = new StringBuffer
    str.append(formatType(t) + " : ")
    str.append(formatType(t.BaseType))
    val ifaces: Array[Type] = t.getInterfaces

    {
      var i: Int = 0
      while (i < ifaces.length) {
        str.append(", " + formatType(ifaces(i)))
        ({
          i += 1;
          i - 1
        })
      }
    }
    return str.toString
  }

  private var MSCORLIB: Assembly = null
  private var MSCORLIB_DLL: Module = null

  def OBJECT: Type = {
    return __OBJECT
  }

  def STRING: Type = {
    return __STRING
  }

  def ARRAY: Type = {
    return __ARRAY
  }

  def VOID: Type = {
    return __VOID
  }

  def ENUM: Type = {
    return __ENUM
  }

  def VALUE_TYPE: Type = {
    return __VALUE_TYPE
  }

  private var __OBJECT: Type = null
  private var __STRING: Type = null
  private var __ARRAY: Type = null
  private var __VOID: Type = null
  private var __ENUM: Type = null
  private var __VALUE_TYPE: Type = null

  def initMSCORLIB(mscorlib: Assembly): Unit = {
    if (MSCORLIB == null) {
      MSCORLIB = mscorlib
      MSCORLIB_DLL = MSCORLIB.modules.apply(0)
      __OBJECT = mscorlib.getType("System.Object")
      __STRING = mscorlib.getType("System.String")
      __ARRAY = mscorlib.getType("System.Array")
      __VOID = mscorlib.getType("System.Void")
      __ENUM = mscorlib.getType("System.Enum")
      __VALUE_TYPE = mscorlib.getType("System.ValueType")
    }
  }
}

abstract class Type(val module: Module, val attributes: Int, _fullName: String, var baseType: Type, var interfaces: Array[Type], override val declaringType: Type, val auxAttr: Int, val elemType: Type) extends MemberInfo(if (_fullName.lastIndexOf(Type.Delimiter) < 0) _fullName else _fullName.substring(_fullName.lastIndexOf(Type.Delimiter) + 1, _fullName.length), declaringType) {
  val (fullName: String, namespace: String) =
    if (declaringType == null) {
      val i: Int = _fullName.lastIndexOf(Type.Delimiter)
      (_fullName, if ((i < 0)) "" else _fullName.substring(0, i))
    }
    else {
      (s"${declaringType.fullName}+${_fullName}", declaringType.namespace)
    }
  override val name = if (_fullName.lastIndexOf(Type.Delimiter) < 0) _fullName else _fullName.substring(_fullName.lastIndexOf(Type.Delimiter) + 1, _fullName.length)

  private final val tVars: List[GenericParamAndConstraints] = new java.util.LinkedList[GenericParamAndConstraints]
  private var sortedTVars: Array[GenericParamAndConstraints] = null

  def addTVar(tvarAndConstraints: GenericParamAndConstraints): Unit = {
    sortedTVars = null
    tVars.add(tvarAndConstraints)
  }

  def getSortedTVars: Array[GenericParamAndConstraints] = {
    if (sortedTVars == null) {
      sortedTVars = new Array[GenericParamAndConstraints](tVars.size)

      {
        var i: Int = 0
        while (i < sortedTVars.length) {
          {
            val iter: Iterator[GenericParamAndConstraints] = tVars.iterator
            while (iter.hasNext) {
              val tvC: GenericParamAndConstraints = iter.next
              if (tvC.Number == i) {
                sortedTVars(i) = tvC
              }
            }
          }
          ({
            i += 1;
            i - 1
          })
        }
      }
    }
    return sortedTVars
  }

  /** The type from which the current Type directly inherits. */
  final def BaseType: Type = {
    doInitBaseType()
    baseType
  }

  /** The assembly that the type is declared in. */
  final def assembly: Assembly = module.assembly

  final def memberType: Int = if (declaringType == null) MemberTypes.TypeInfo else MemberTypes.NestedType

  protected var fields: Array[FieldInfo] = null
  protected var methods: Array[MethodInfo] = null
  protected var constructors: Array[ConstructorInfo] = null
  protected var properties: Array[PropertyInfo] = null
  protected var events: Array[EventInfo] = null
  protected var nestedTypes: Array[Type] = null
  protected var underlyingType: Type = null

  final def isAbstract: Boolean = (attributes & TypeAttributes.Abstract) != 0

  final def isPublic: Boolean = (attributes & TypeAttributes.VisibilityMask) == TypeAttributes.Public

  final def isNotPublic: Boolean = (attributes & TypeAttributes.VisibilityMask) == TypeAttributes.NotPublic

  final def isNestedPublic: Boolean = (attributes & TypeAttributes.VisibilityMask) == TypeAttributes.NestedPublic

  final def isNestedPrivate: Boolean = (attributes & TypeAttributes.VisibilityMask) == TypeAttributes.NestedPrivate

  final def isNestedFamily: Boolean = (attributes & TypeAttributes.VisibilityMask) == TypeAttributes.NestedFamily

  final def isNestedAssembly: Boolean = (attributes & TypeAttributes.VisibilityMask) == TypeAttributes.NestedAssembly

  final def isNestedFamORAssem: Boolean = (attributes & TypeAttributes.VisibilityMask) == TypeAttributes.NestedFamORAssem

  final def isNestedFamANDAssem: Boolean = (attributes & TypeAttributes.VisibilityMask) == TypeAttributes.NestedFamANDAssem

  final def isSealed: Boolean = (attributes & TypeAttributes.Sealed) != 0

  final def isSpecialName: Boolean = (attributes & TypeAttributes.SpecialName) != 0

  final def isClass: Boolean = (attributes & TypeAttributes.ClassSemanticsMask) == TypeAttributes.Class

  final def isInterface: Boolean = (attributes & TypeAttributes.ClassSemanticsMask) == TypeAttributes.Interface

  final def isAutoLayout: Boolean = (attributes & TypeAttributes.LayoutMask) == TypeAttributes.AutoLayout

  final def isExplictitLayout: Boolean = (attributes & TypeAttributes.LayoutMask) == TypeAttributes.ExplicitLayout

  final def isLayoutSequential: Boolean = (attributes & TypeAttributes.LayoutMask) == TypeAttributes.SequentialLayout

  final def isImport: Boolean = (attributes & TypeAttributes.Import) != 0

  final def isSerializable: Boolean = (attributes & TypeAttributes.Serializable) != 0

  final def isAnsiClass: Boolean = (attributes & TypeAttributes.StringFormatMask) == TypeAttributes.AnsiClass

  final def isUnicodeClass: Boolean = (attributes & TypeAttributes.StringFormatMask) == TypeAttributes.UnicodeClass

  final def isAutoClass: Boolean = (attributes & TypeAttributes.StringFormatMask) == TypeAttributes.AutoClass

  final def isArray: Boolean = (auxAttr & Type.AuxAttr.Array) != 0

  final def isByRef: Boolean = (auxAttr & Type.AuxAttr.ByRef) != 0

  final def isPointer: Boolean = (auxAttr & Type.AuxAttr.Pointer) != 0

  final def isPrimitive: Boolean = (auxAttr & Type.AuxAttr.Primitive) != 0

  final def isValueType: Boolean = (BaseType eq Type.VALUE_TYPE) || isEnum

  final def isEnum: Boolean = BaseType eq Type.ENUM

  def canBeTakenAddressOf: Boolean = isValueType && (this ne Type.ENUM)

  /**
   * IsGeneric, true for a PEType or TypeBuilder (i.e., a type definition)
   * containing one or more type params. Not to be called on a reference to a
   * constructed type.
   */
  final def isGeneric: Boolean = tVars.size > 0

  final def hasElementType: Boolean = isArray || isPointer || isByRef

  def isTMVarUsage: Boolean = false

  def isNestedType: Boolean = declaringType != null

  def isDefinitelyInternal: Boolean =
    if (isNestedType) isNestedPrivate
    else isNotPublic

  protected def this(module: Module, attr: Int, fullName: String, baseType: Type, interfaces: Array[Type], declType: Type, auxAttr: Int) {
    this(module, attr, fullName, baseType, interfaces, declType, auxAttr, null)
  }

  protected def this(instantiatedType: Type) {
    this(instantiatedType.module, instantiatedType.attributes, "", null, null, null, instantiatedType.auxAttr, null)
  }

  /**
   * @return the type of the object encompassed or referenced to by the
   *         current array, pointer or reference type.
   */
  def getElementType: Type = elemType

  /**
   * @return the type underlying an enumeration type.
   */
  def getUnderlyingType: Type = {
    if (!isEnum) return null
    doInitFields()
    underlyingType
  }

  /** Searches for the field with the specified name. */
  def getField(name: String): FieldInfo = {
    doInitFields()

    var i: Int = 0
    while (i < fields.length) {
      if ((fields(i).name == name) && !fields(i).isPrivate) return fields(i)
      i += 1
    }
    null
  }

  /**
    */
  def getField(name: String, bindingFlags: Int): FieldInfo = {
    val fields: Array[FieldInfo] = this.getFields(bindingFlags)

    var i: Int = 0
    while (i < fields.length) {
      if (name == fields(i).name) return fields(i)
      i += 1
    }
    null
  }

  /** Gets the fields of the current Type. */
  def getFieldsArray: Array[FieldInfo] = getFields(BindingFlags.Instance | BindingFlags.Public)

  /**
    */
  def getFields(bindingFlags: Int): Array[FieldInfo] = {
    doInitFields()
    val fields: Array[FieldInfo] = getAllFields((bindingFlags & BindingFlags.DeclaredOnly) != 0)
    val getInstance: Boolean = (bindingFlags & BindingFlags.Instance) != 0
    val getStatic: Boolean = (bindingFlags & BindingFlags.Static) != 0
    val getPublic: Boolean = (bindingFlags & BindingFlags.Public) != 0
    val getNonPublic: Boolean = (bindingFlags & BindingFlags.NonPublic) != 0
    var cnt: Int = 0

    {
      var i: Int = 0
      while (i < fields.length) {
        val field: FieldInfo = fields(i)
        val accessible: Boolean = (getPublic && field.isPublic) || (getNonPublic && !field.isPublic)
        if (accessible && ((field.declaringType eq this) || ((field.declaringType ne this) && !field.isPrivate)) && ((getInstance && !field.isStatic) || ((getStatic && field.isStatic) && ((field.declaringType eq this) || ((bindingFlags & BindingFlags.FlattenHierarchy) != 0))))) fields(({
          cnt += 1;
          cnt - 1
        })) = field

        i += 1
      }
    }
    val resFields: Array[FieldInfo] = new Array[FieldInfo](cnt)
    System.arraycopy(fields, 0, resFields, 0, cnt)
    resFields
  }

  protected def getAllFields(declaredOnly: Boolean): Array[FieldInfo] = {
    doInitFields()
    val inherited: Array[FieldInfo] = if (BaseType == null || declaredOnly) Array.empty[FieldInfo] else BaseType.getAllFields(declaredOnly)
    val allFields: Array[FieldInfo] = new Array[FieldInfo](inherited.length + this.fields.length)
    System.arraycopy(inherited, 0, allFields, 0, inherited.length)
    System.arraycopy(this.fields, 0, allFields, inherited.length, this.fields.length)
    allFields
  }

  /**
   * Searches for a public instance constructor whose parameters match the
   * types in the specified array.
   */
  def getConstructor(paramTypes: Array[Type]): ConstructorInfo = {
    doInitMethods()

    {
      var i: Int = 0
      while (i < constructors.length) {
        if (Type.equalParameters(constructors(i).getParameters, paramTypes)) return constructors(i)
        i += 1
      }
    }
    return null
  }

  /** Returns all public instance constructors defined for the current Type. */
  def getConstructorsArray: Array[ConstructorInfo] = getConstructors(BindingFlags.Instance | BindingFlags.Public)

  /** */
  def getConstructors(bindingFlags: Int): Array[ConstructorInfo] = {
    doInitMethods()
    val getInstance: Boolean = (bindingFlags & BindingFlags.Instance) != 0
    val getStatic: Boolean = (bindingFlags & BindingFlags.Static) != 0
    val getPublic: Boolean = (bindingFlags & BindingFlags.Public) != 0
    val getNonPublic: Boolean = (bindingFlags & BindingFlags.NonPublic) != 0
    val constrs: Array[ConstructorInfo] = new Array[ConstructorInfo](this.constructors.length)
    var cnt: Int = 0

    {
      var i: Int = 0
      while (i < this.constructors.length) {
        {
          val constr: ConstructorInfo = this.constructors(i)
          val accessible: Boolean = (getPublic && constr.isPublic) || (getNonPublic && !constr.isPublic)
          if (accessible && ((getInstance && !constr.isStatic) || (getStatic && constr.isStatic))) constrs(({
            cnt += 1;
            cnt - 1
          })) = constr
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
    val resConstrs: Array[ConstructorInfo] = new Array[ConstructorInfo](cnt)
    System.arraycopy(constrs, 0, resConstrs, 0, cnt)
    resConstrs
  }

  /**
   * Searches for the specified public method whose parameters match the
   * specified argument types.
   */
  def getMethod(name: String, paramTypes: Array[Type]): MethodInfo = getMethod(name, paramTypes, null)

  def getMethod(name: String, paramTypes: Array[Type], retType: Type): MethodInfo = {
    doInitMethods()
    var method: MethodInfo = Type.findMethod(methods, name, paramTypes, retType)
    if (method != null) return method
    if (BaseType != null) {
      method = BaseType.getMethod(name, paramTypes, retType)
      if (method != null) return method
    }
    return null
  }

  /**
    */
  def getMethod(name: String, paramTypes: Array[Type], bindingFlags: Int): MethodInfo = {
    val methods: Array[MethodInfo] = getMethods(bindingFlags)
    val method: MethodInfo = Type.findMethod(methods, name, paramTypes, null)
    if (method == null) {
      val str: StringBuffer = new StringBuffer(name)
      str.append('(')

      {
        var i: Int = 0
        while (i < paramTypes.length) {
          if (i > 0) str.append(", ")
          str.append(paramTypes(i))
          i += 1
        }
      }
      str.append(')')
      System.out.println("Cannot find method " + str + ":")
      System.out.println("Methods of class " + this)

      {
        var i: Int = 0
        while (i < methods.length) {
          System.out.println("\t" + methods(i))
          i += 1
        }
      }
    }
    method
  }

  /** Returns all public methods of the current Type. */
  def getMethodsArray: Array[MethodInfo] = getMethods(BindingFlags.Instance | BindingFlags.Public)

  /**
    */
  def getMethods(bindingFlags: Int): Array[MethodInfo] = {
    doInitMethods()
    val methods: Array[MethodInfo] = getAllMethods((bindingFlags & BindingFlags.DeclaredOnly) != 0)
    val getInstance: Boolean = (bindingFlags & BindingFlags.Instance) != 0
    val getStatic: Boolean = (bindingFlags & BindingFlags.Static) != 0
    val getPublic: Boolean = (bindingFlags & BindingFlags.Public) != 0
    val getNonPublic: Boolean = (bindingFlags & BindingFlags.NonPublic) != 0
    var cnt: Int = 0

    {
      var i: Int = 0
      while (i < methods.length) {
        {
          val method: MethodInfo = methods(i)
          val accessible: Boolean = (getPublic && method.isPublic) || (getNonPublic && !method.isPublic)
          if (accessible
            && ((method.declaringType eq this) || ((method.declaringType ne this) && !method.isPrivate))
            && ((getInstance && !method.isStatic) ||
            ((getStatic && method.isStatic) && ((method.declaringType eq this) || ((bindingFlags & BindingFlags.FlattenHierarchy) != 0))))) methods(({
            cnt += 1;
            cnt - 1
          })) = method
        }
        i += 1
      }
    }
    val resMethods: Array[MethodInfo] = new Array[MethodInfo](cnt)
    System.arraycopy(methods, 0, resMethods, 0, cnt)
    return resMethods
  }

  protected def getAllMethods(declaredOnly: Boolean): Array[MethodInfo] = {
    doInitMethods()
    val inherited: Array[MethodInfo] = if (BaseType == null || declaredOnly) Array.empty[MethodInfo] else BaseType.getAllMethods(declaredOnly)
    val allMethods: Array[MethodInfo] = new Array[MethodInfo](inherited.length + this.methods.length)
    System.arraycopy(inherited, 0, allMethods, 0, inherited.length)
    System.arraycopy(this.methods, 0, allMethods, inherited.length, this.methods.length)
    return allMethods
  }

  /**
   * Returns all public properties of the current Type.
   */
  def getPropertiesArray: Array[PropertyInfo] = {
    doInitProperties()
    properties.clone
  }

  /**
   * Returns the properties of the current class that satisfy the binding
   * constrints.
   */
  def getProperties(bindingFlags: Int): Array[PropertyInfo] = {
    doInitProperties()
    properties.clone
  }

  /**
   * Returns the public property with the given name.
   */
  def getProperty(name: String): PropertyInfo = {
    doInitProperties()

    {
      var i: Int = 0
      while (i < properties.length) {
        if (name == properties(i).name) return properties(i)
        ({
          i += 1;
          i - 1
        })
      }
    }
    return null
  }

  /**
   * Returns the property with the given name that satisfies the binding
   * constraints.
   */
  def getProperty(name: String, bindingFlags: Int): PropertyInfo = {
    throw new RuntimeException("Method not implemented yet")
  }

  def getEvents: Array[EventInfo] = {
    doInitEvents()
    events.clone
  }

  /** Searches for nested type with the specified name. */
  def getNestedType(name: String): Type = {
    doInitNestedTypes()

    var i: Int = 0
    while (i < nestedTypes.length) {
      if (nestedTypes(i).name == name) return nestedTypes(i)
      i += 1
    }
    null
  }

  /** Returns all types nested within the current Type. */
  def getNestedTypes: Array[Type] = {
    doInitNestedTypes()
    nestedTypes.clone
  }

  /**
   * Searches for an Interface with the given name implemented by this type
   */
  def getInterface(name: String): Type = getInterface(name, false)

  /**
   * Searches for the specified interface, specifying whether to do a
   * case-sensitive search.
   *
   * @param name
     * - the name of the interface to get
   * @param ignoreCase
     * <b>true</b> to perform a case-insensitive search for name
   *   <b>false</b> to perform a case-sensitive search for name
   * @return A Type object representing the interface with the specified name,
   *         implemented or inherited by the current Type, if found;
   *         otherwise, a null reference
   */
  def getInterface(name: String, ignoreCase: Boolean): Type = {
    doInitInterfaces()

    {
      var i: Int = 0
      while (i < interfaces.length) {
        {
          val iface: Type = interfaces(i)
          if (ignoreCase) {
            if (name.equalsIgnoreCase(iface.name)) return iface
            if (name.equalsIgnoreCase(iface.fullName)) return iface
          }
          else {
            if (name == iface.name) return iface
            if (name == iface.fullName) return iface
          }
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
    if (BaseType == null) null else BaseType.getInterface(name, ignoreCase)
  }

  /** Returns the interfaces implemented or inherited by the current Type. */
  def getInterfaces: Array[Type] = {
    doInitInterfaces()
    if (BaseType == null) return interfaces
    val ifaces: Array[Type] = interfaces
    var count: Int = 0

    {
      var i: Int = 0
      while (i < interfaces.length) {
        {
          if (BaseType.getInterface(interfaces(i).fullName) == null) ifaces(({
            count += 1;
            count - 1
          })) = ifaces(i)
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
    val baseTypeIfaces: Array[Type] = BaseType.getInterfaces
    val res: Array[Type] = new Array[Type](baseTypeIfaces.length + count)
    System.arraycopy(baseTypeIfaces, 0, res, 0, baseTypeIfaces.length)
    System.arraycopy(ifaces, 0, res, baseTypeIfaces.length, count)
    res
  }

  def isSubtypeOf(that: Type): Boolean = {
    if ((this eq that) || (BaseType eq that) || (that eq Type.OBJECT)) return true
    doInitInterfaces()

    if (interfaces.exists(_.isSubtypeOf(that))) true
    else if (BaseType == null) false else BaseType.isSubtypeOf(that)
  }

  protected var members: Array[MemberInfo] = null

  def getMember(name: String): Array[MemberInfo] = {
    aggregateMembers()
    members.filter(member => name == member.name)
  }

  protected def aggregateMembers(): Unit = {
    if (members != null) return
    doInitFields()
    doInitMethods()
    doInitProperties()
    doInitNestedTypes()
    val l: java.util.List[MemberInfo] = new ArrayList[MemberInfo]()
    l.addAll(Arrays.asList(fields: _*))
    l.addAll(Arrays.asList(constructors: _*))
    l.addAll(Arrays.asList(methods: _*))
    l.addAll(Arrays.asList(properties: _*))
    l.addAll(Arrays.asList(nestedTypes: _*))
    members = l.toArray(Array.empty[MemberInfo])
  }

  /**
   * Return only the fields declared in this type.
   */
  def getDeclaredFields: Array[FieldInfo] = {
    doInitFields()
    val fields: Array[FieldInfo] = new Array[FieldInfo](this.fields.length)
    System.arraycopy(this.fields, 0, fields, 0, fields.length)
    return fields
  }

  /**
   * Return only the constructors declared in this type.
   */
  def getDeclaredConstructors: Array[ConstructorInfo] = {
    doInitMethods()
    val ctors: Array[ConstructorInfo] = new Array[ConstructorInfo](constructors.length)
    System.arraycopy(constructors, 0, ctors, 0, ctors.length)
    return ctors
  }

  /**
   * Return only the methods declared in this type.
   */
  def getDeclaredMethods: Array[MethodInfo] = {
    doInitMethods()
    val methods: Array[MethodInfo] = new Array[MethodInfo](this.methods.length)
    System.arraycopy(this.methods, 0, methods, 0, methods.length)
    methods
  }

  /**
   * Return only the properties declared in this type.
   */
  def getDeclaredProperties: Array[PropertyInfo] = {
    doInitProperties()
    val props: Array[PropertyInfo] = new Array[PropertyInfo](properties.length)
    System.arraycopy(properties, 0, props, 0, props.length)
    props
  }

  /**
   * Return only the interfaces directly implemented by this type.
   */
  def getDeclaredInterfaces: Array[Type] = {
    doInitInterfaces()
    val ifaces: Array[Type] = new Array[Type](interfaces.length)
    System.arraycopy(interfaces, 0, ifaces, 0, ifaces.length)
    ifaces
  }

  /**
   * Return the types declared in this type.
   */
  def getDeclaredNestedTypes: Array[Type] = {
    doInitNestedTypes()
    val nested: Array[Type] = new Array[Type](nestedTypes.length)
    System.arraycopy(nestedTypes, 0, nested, 0, nested.length)
    nested
  }

  override def toString: String = fullName

  private var initBaseType: Boolean = true

  protected final def doInitBaseType(): Unit = {
    if (initBaseType) {
      loadBaseType
      initBaseType = false
    }
  }

  protected def loadBaseType(): Unit = {
  }

  private var initInterfaces: Boolean = true

  protected def doInitInterfaces(): Unit = {
    if (initInterfaces) {
      loadInterfaces
      initInterfaces = false
    }
    assert(interfaces != null, "In type " + this)
  }

  protected def loadInterfaces(): Unit = {
  }

  private var initNestedTypes: Boolean = true

  protected def doInitNestedTypes(): Unit = {
    if (initNestedTypes) {
      loadNestedTypes
      initNestedTypes = false
    }
    assert(nestedTypes != null, "In type " + this)
  }

  protected def loadNestedTypes(): Unit = {
  }

  private var initFields: Boolean = true

  protected def doInitFields(): Unit = {
    if (initFields) {
      loadFields
      initFields = false
    }
    assert(fields != null, "In type " + this)
  }

  protected def loadFields(): Unit = {}

  private var initMethods: Boolean = true

  private[cilly] def doInitMethods(): Unit = {
    if (initMethods) {
      loadMethods
      initMethods = false
    }
    assert(constructors != null, "In type " + this)
    assert(methods != null, "In type " + this)
  }

  protected def loadMethods(): Unit = {}

  private var initProperties: Boolean = true

  protected def doInitProperties(): Unit = {
    if (initProperties) {
      doInitMethods()
      loadProperties()
      initProperties = false
    }
    assert(properties != null, "In type " + this)
  }

  protected def loadProperties(): Unit = {}

  private var initEvents: Boolean = true

  protected def doInitEvents(): Unit = {
    if (initEvents) {
      doInitMethods()
      loadEvents
      initEvents = false
    }
    assert(events != null, "In type " + this)
  }

  protected def loadEvents(): Unit = {}
}