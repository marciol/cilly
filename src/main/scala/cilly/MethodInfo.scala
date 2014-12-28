/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/**
 * Discovers the attributes of a method and provides access to method metadata.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class MethodInfo(override val name: String, override val declaringType: Type, override val attributes: Short, val returnType: Type, override val params: Array[ParameterInfo]) extends MethodBase(name, declaringType, attributes, params) {

  def this(name: String, declaringType: Type, attrs: Short, returnType: Type, params: Array[Type]) {
    this(name, declaringType, attrs, returnType, MethodBase.convertParamTypesToParameterInfos(params))
  }

  override def hasPtrParamOrRetType: Boolean = {
    if (returnType.isByRef && !returnType.getElementType.isValueType) true
    else if (returnType.isPointer) true
    else super.hasPtrParamOrRetType
  }

  final def memberType: Int = MemberTypes.Method

  final def isConstructor: Boolean = false

  override def toString: String = s"${MethodAttributes.toString(attributes)} $returnType $declaringType::$name$params2String"
}
