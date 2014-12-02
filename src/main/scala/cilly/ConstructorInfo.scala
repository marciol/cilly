/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly;

/** Discovers the attributes of a class constructor and provides access to
  * constructor metadata. ConstructorInfo is used to discover the attributes of a
  * constructor as well as to invoke a constructor. Objects are created by
  * invoking either the GetConstructors or GetConstructor method of a Type
  * object.
  *
  * @author Nikolay Mihaylov
  * @version 1.0
  */
class ConstructorInfo(name: String, declType: Type, attrs: Int, params: Array[ParameterInfo])
    extends MethodBase(name, declType, attrs, params) {

  assert(declType != null, "Owner can't be 'null' for a constructor!")

  final def memberType = MemberTypes.Constructor

  override final def isConstructor = true

  /** Public constructors */

  def this(declType: Type, attrs: Int, paramTypes: Array[Type]) {
    this(ConstructorInfo.getName(attrs), declType, attrs, MethodBase.convertParamTypesToParameterInfos(paramTypes))
  }

  def this(declType: Type, attrs: Int, params: Array[ParameterInfo]) {
    this(ConstructorInfo.getName(attrs), declType, attrs, params);
  }

  override def toString =
    s"${MethodAttributes.toString(Attributes)} ${Type.VOID} ${declaringType.fullName}::$name$params2String"
}

object ConstructorInfo {
  final val CTOR = ".ctor";
  final val CCTOR = ".cctor";
  final val EMPTY_ARRAY: Array[ConstructorInfo] = new Array[ConstructorInfo](0)

  final def getName(attrs: Int) = if ((attrs & MethodAttributes.Static) == 0) CTOR else CCTOR
}
