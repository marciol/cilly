/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.util.LinkedList
import java.util.Iterator
import java.util.List

object MethodBase {
  def convertParamTypesToParameterInfos(paramTypes: Array[Type]): Array[ParameterInfo] = {
    assert(paramTypes != null)
    val params: Array[ParameterInfo] = new Array[ParameterInfo](paramTypes.length)

    {
      var i: Int = 0
      while (i < params.length) {
        params(i) = new ParameterInfo(null, paramTypes(i), 0, i)
        ({
          i += 1; i - 1
        })
      }
    }
    params
  }
}

/**
 * The common superclass of MemberInfo and ConstructorInfo
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
abstract class MethodBase(override val name: String, override val declaringType: Type, attrs: Short, val params: Array[ParameterInfo]) extends MemberInfo(name, declaringType) {
  val attributes: Short =
    if (isConstructor) (attrs | MethodAttributes.SpecialName | MethodAttributes.RTSpecialName).toShort
    else attrs
  val callingConvention: Short = (CallingConventions.Standard | (if (isStatic) 0.toShort else CallingConventions.HasThis)).toShort

  private final val mVars: List[GenericParamAndConstraints] = new LinkedList[GenericParamAndConstraints]
  private var sortedMVars: Array[GenericParamAndConstraints] = null

  def addMVar(tvarAndConstraints: GenericParamAndConstraints): Unit = {
    sortedMVars = null
    mVars.add(tvarAndConstraints)
  }

  def getSortedMVars: Array[GenericParamAndConstraints] = {
    if (sortedMVars == null) {
      sortedMVars = new Array[GenericParamAndConstraints](mVars.size)

      {
        var i: Int = 0
        while (i < sortedMVars.length) {
          {
            val iter: Iterator[GenericParamAndConstraints] = mVars.iterator
            while (iter.hasNext) {
              val tvC: GenericParamAndConstraints = iter.next
              if (tvC.Number == i) {
                sortedMVars(i) = tvC
              }
            }
          }
          ({
            i += 1; i - 1
          })
        }
      }
    }
    return sortedMVars
  }

  final def isGeneric: Boolean = mVars.size > 0

  def isConstructor: Boolean

  final def isAbstract: Boolean = (attributes & MethodAttributes.Abstract) != 0

  final def isFinal: Boolean = (attributes & MethodAttributes.Final) != 0

  final def isVirtual: Boolean = (attributes & MethodAttributes.Virtual) != 0

  final def isInstance: Boolean = !isStatic && !isVirtual

  final def isStatic: Boolean = (attributes & MethodAttributes.Static) != 0

  final def isHideBySig: Boolean = (attributes & MethodAttributes.HideBySig) != 0

  final def isSpecialName: Boolean = (attributes & MethodAttributes.SpecialName) != 0

  final def isPublic: Boolean = (attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.Public

  final def isPrivate: Boolean = (attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.Private

  final def isFamily: Boolean = (attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.Family

  final def isAssembly: Boolean = (attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.Assembly

  final def isFamilyOrAssembly: Boolean = (attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.FamORAssem

  final def isFamilyAndAssembly: Boolean = (attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.FamANDAssem

  def hasPtrParamOrRetType: Boolean = {
    val ps: Array[ParameterInfo] = getParameters

    {
      var i: Int = 0
      while (i < ps.length) {
        {
          val pT: Type = ps(i).parameterType
          if (pT.isPointer) {
            return true
          }
          if (pT.isByRef && !pT.getElementType.canBeTakenAddressOf) {
            return true
          }
        }
        ({
          i += 1; i - 1
        })
      }
    }
    return false
  }

  /** Returns the parameters of the method/constructor. */
  def getParameters: Array[ParameterInfo] = params.clone

  def getMethodImplementationFlags: Int = implAttributes

  /** Method parameters. */
  protected var implAttributes: Short = 0

  /*
  protected def this(name: String, declType: Type, attrs: Int, paramTypes: Array[Type]) {
    this()
    `this`(name, declType, attrs)
    params = MethodBase.convertParamTypesToParameterInfos(paramTypes)
  }
  */


  protected def params2String: String = {
    val s: StringBuffer = new StringBuffer("(")

    {
      var i: Int = 0
      while (i < params.length) {
        {
          if (i > 0) s.append(", ")
          s.append(params(i).parameterType)
        }
        ({
          i += 1; i - 1
        })
      }
    }
    s.append(")")
    return s.toString
  }
}