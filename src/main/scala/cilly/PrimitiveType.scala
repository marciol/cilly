package cilly

import cilly.util.PECustomMod

final class PrimitiveType(module: Module, attributes: Int, fullName: String, baseType: Type, interfaces: Array[Type], declType: Type, auxAttr: Int, elemType: Type) extends Type(module, attributes, fullName, baseType, interfaces, declType, auxAttr, elemType) {

  def clearMembers(): Unit = {
    fields = Array.empty
    methods = Array.empty
    constructors = ConstructorInfo.EMPTY_ARRAY
    events = EventInfo.EMPTY_ARRAY
    doInitBaseType()
    doInitInterfaces()
    doInitFields()
    doInitMethods()
    doInitEvents()
    doInitProperties()
    doInitNestedTypes()
  }

  def addField(name: String, attrs: Int, fieldType: Type): FieldInfo = {
    val fieldTypeWithMods: PECustomMod = new PECustomMod(fieldType, null)
    val res: FieldInfo = new FieldInfo(name, this, attrs.toShort, fieldTypeWithMods, null)
    val ms: Array[FieldInfo] = new Array[FieldInfo](fields.length + 1)
    System.arraycopy(fields, 0, ms, 0, fields.length)
    ms(ms.length - 1) = res
    fields = ms
    res
  }

  def addMethod(name: String, attrs: Int, returnType: Type, paramTypes: Array[Type]): MethodInfo = {
    val res: MethodInfo = new MethodInfo(name, this, attrs.toShort, returnType, paramTypes)
    val ms: Array[MethodInfo] = new Array[MethodInfo](methods.length + 1)
    System.arraycopy(methods, 0, ms, 0, methods.length)
    ms(ms.length - 1) = res
    res
  }

  def addConstructor(attrs: Int, paramTypes: Array[Type]): ConstructorInfo = {
    val res: ConstructorInfo = new ConstructorInfo(this, attrs.toShort, paramTypes)
    val ms: Array[ConstructorInfo] = new Array[ConstructorInfo](constructors.length + 1)
    System.arraycopy(constructors, 0, ms, 0, constructors.length)
    ms(ms.length - 1) = res
    res
  }
}