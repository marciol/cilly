/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.util.{Iterator, LinkedList, List}

/**
 * @author Nikolay Mihaylov
 * @version 1.0
 */
abstract class CustomAttributeProvider extends ICustomAttributeProvider {
  protected var custAttrs: List[Attribute] = null

  def getCustomAttributes(inherit: Boolean): Array[AnyRef] = {
    initAttributes(null)
    return if (custAttrs.size == 0) Array.emptyObjectArray else custAttrs.toArray(new Array[AnyRef](custAttrs.size))
  }

  def getCustomAttributes(attributeType: Type, inherit: Boolean): Array[AnyRef] = {
    initAttributes(attributeType)
    var tAttrs: List[Attribute] = null
    if (constrType eq attributeType) tAttrs = custAttrs
    else {
      tAttrs = new LinkedList[Attribute]
      {
        val attrs: Iterator[Attribute] = custAttrs.iterator
        while (attrs.hasNext) {
          val a: Attribute = attrs.next
          if (a.getType eq attributeType) tAttrs.add(a)
        }
      }
    }
    return if (tAttrs.size == 0) Array.emptyObjectArray else tAttrs.toArray(new Array[AnyRef](tAttrs.size))
  }

  def isDefined(attributeType: Type, inherit: Boolean): Boolean = {
    initAttributes(attributeType)
    if (constrType eq attributeType) return custAttrs.size > 0
    val attrs: Iterator[Attribute] = custAttrs.iterator
    while (attrs.hasNext) {
      if (attrs.next.getType eq attributeType) return true
    }
    return false
  }

  private[cilly] def addCustomAttribute(constr: ConstructorInfo, value: Array[Byte]): Unit = {
    val attr: Attribute = new Attribute(constr, value)
    assert(constrType == null || (constrType eq attr.getType))
    if (custAttrs == null) custAttrs = new LinkedList[Attribute]
    custAttrs.add(attr)
  }

  private def initAttributes(atype: Type): Unit = {
    if (custAttrs != null && (constrType == null || (constrType eq atype))) return
    custAttrs = new LinkedList[Attribute]
    constrType = atype
    loadCustomAttributes(atype)
  }

  protected def loadCustomAttributes(atype: Type): Unit = {
  }

  private var constrType: Type = null
}