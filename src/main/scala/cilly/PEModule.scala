/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.io.{File, IOException}
import java.nio.ByteBuffer

import cilly.util.{Signature, Table}
import cilly.util.Table._

/**
 * Represents a module corresponding to a PE/COFF file
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final class PEModule(val pefile: PEFile, val definingRow: Int, scopeName: String, assem: Assembly) extends Module(pefile.name, pefile.absolutePath, scopeName, assem) {
  pefile.initModule(this)
  pefile.typeDef.load
  loadGlobals()

  private var typeRefs: Array[Type] = null

  override def getType(typeName: String): Type = {
    doInitTypes
    val o: AnyRef = typesMap.get(typeName)
    if (o == null) {
      return null
    }
    // WTF
    return if (o.isInstanceOf[Type]) o.asInstanceOf[Type] else getTypeDef((o.asInstanceOf[Integer]).intValue)
  }

  /**
   * Load information about the types defined in this module.
   */
  protected override def loadTypes(): Unit = {
    typeRefs = new Array[Type](pefile.typeRef.rows)
    val nbTypes: Int = pefile.typeDef.rows

    {
      var row: Int = 2
      while (row <= nbTypes) {
        {
          val name: String = pefile.typeDef(row).getFullName
          throw new UnsupportedOperationException("Trying to add a value of type Integer instead of Type")
        }
        ({
          row += 1; row - 1
        })
      }
    }
    this.types = new Array[Type](nbTypes - 1)

    {
      var row: Int = 2
      while (row <= nbTypes) {
        {
          getTypeDef(row)
        }
        ({
          row += 1; row - 1
        })
      }
    }
  }

  /**
   * Return the type defined at the given row in the TypeDef table.
   */
  private[cilly] def getTypeDef(row: Int): Type = {
    if (this.types(row - 2) != null) return this.types(row - 2)
    val `type`: Table.TypeDef = pefile.typeDef(row)
    val attrs: Int = `type`.Flags
    val name: String = `type`.getFullName
    var declType: Type = null
    if (TypeAttributes.isNested(attrs)) {
      {
        var i: Int = 1
        while (i <= pefile.NestedClass.rows) {
          {
            pefile.NestedClass.readRow(i)
            if (pefile.NestedClass.nestedClass == row) declType = getTypeDef(pefile.NestedClass.enclosingClass)
          }
          ({
            i += 1; i - 1
          })
        }
      }
    }
    val t: Type = new PEType(this, attrs, name, declType, Type.AuxAttr.None, pefile, row)
    types(row - 2) = t
    addType(t)
    val tvarIdxes: Array[Int] = pefile.genericParam.getTVarIdxes(row)

    {
      var i: Int = 0
      while (i < tvarIdxes.length) {
        {
          val tvarAndConstraints: GenericParamAndConstraints = getTypeConstraints(tvarIdxes(i))
          t.addTVar(tvarAndConstraints)
        }
        ({
          i += 1; i - 1
        })
      }
    }
    return t
  }

  def getTypeConstraints(genParamIdx: Int): GenericParamAndConstraints = {
    val tvarNumber: Int = pefile.genericParam(genParamIdx).number
    val tvarName: String = pefile.genericParam.getName
    val isInvariant: Boolean = pefile.genericParam.isInvariant
    val isCovariant: Boolean = pefile.genericParam.isCovariant
    val isContravariant: Boolean = pefile.genericParam.isContravariant
    val isReferenceType: Boolean = pefile.genericParam.isReferenceType
    val isValueType: Boolean = pefile.genericParam.isValueType
    val hasDefaultConstructor: Boolean = pefile.genericParam.hasDefaultConstructor
    val TypeDefOrRefIdxes: Array[Int] = pefile.genericParamConstraint.getTypeDefOrRefIdxes(genParamIdx)
    val tCtrs: Array[Type] = new Array[Type](TypeDefOrRefIdxes.length)

    {
      var i: Int = 0
      while (i < TypeDefOrRefIdxes.length) {
        {
          val tConstraint: Type = getTypeDefOrRef(TypeDefOrRefIdxes(i))
          tCtrs(i) = tConstraint
        }
        ({
          i += 1; i - 1
        })
      }
    }
    val res: GenericParamAndConstraints = new GenericParamAndConstraints(tvarNumber, tvarName, tCtrs, isInvariant, isCovariant, isContravariant, isReferenceType, isValueType, hasDefaultConstructor)
    return res
  }

  /**
   * Load the description of the module-global fields and methods
   */
  protected override def loadGlobals(): Unit = {
  }

  protected override def loadCustomAttributes(attributeType: Type): Unit = {
    initAttributes(this, 1, Table.ModuleDef.ID, attributeType)
  }

  /**
   * Return the type referenced by the given row in the TypeRef table.
   */
  private[cilly] def getTypeRef(row: Int): Type = getTypeRef(row, null)

  /**
   * Return the type referenced by the given row in the TypeRef table only if it
   * resides in the given assembly. <i>Used by initCustomAttributes to avoid
   * unnecessary loading of referenced assemblies.</i>
   */
  private[cilly] def getTypeRef(row: Int, inAssembly: Assembly): Type = {
    var `type`: Type = typeRefs(row - 1)
    if (`type` != null) return `type`
    val tr: Table.TypeRef = pefile.typeRef
    tr.readRow(row)
    val tableId: Int = Table.tableId(Table._ResolutionScope, tr.ResolutionScope)
    val refRow: Int = tr.ResolutionScope >> Table.NoBits(Table._ResolutionScope)
    val typeName: String = tr.getFullName
    pefile.getTable(tableId).readRow(refRow)
    tableId match {
      case AssemblyRef.ID =>
        val name: String = pefile.AssemblyRef.getName
        if (inAssembly != null && !(inAssembly.getName.name == name)) return null
        val assem: Assembly = getAssembly(name)
        `type` = assem.getType(typeName)
        if (`type` == null) {
          val asmb: Assembly = getAssembly("mscorlib")
          `type` = asmb.getType("System.Object")
        }
      case ModuleDef.ID =>
        assert(refRow == 1)
        `type` = this.getType(typeName)
      case TypeRef.ID =>
        val nestingType: Type = getTypeRef(refRow)
        val nestedName: String = typeName
        `type` = nestingType.getNestedType(nestedName)
      case ModuleRef.ID =>
        `type` = getAssembly(pefile.ModuleRef.getName).getType(typeName)
      case _ =>
        throw new RuntimeException(refRow + "@" + pefile.getTable(tableId).getTableName)
    }
    if (typeRefs(row - 1) != null) System.out.println("TypeRef[" + PEFile.short2hex(row) + "] " + "changing type " + typeRefs(row - 1) + " for type " + `type`)
    typeRefs(row - 1) = `type`
    assert(`type` != null, "Couldn't find type " + typeName)
    return `type`
  }

  private def getAssembly(name: String): Assembly = {
    var assem: Assembly = Assembly.getAssembly(name)
    if (assem != null) return assem
    var dir: File = pefile.parentFile
    assem = Assembly.loadFrom(dir, name)
    if (assem != null) return assem
    try {
      dir = pefile.underlyingFile.getCanonicalFile.getParentFile
    }
    catch {
      case e: IOException => {
        throw new RuntimeException(e)
      }
    }
    assem = Assembly.loadFrom(dir, name)
    if (assem != null) return assem
    throw new RuntimeException("Cannot find assembly: " + name)
  }

  /**
   * Return the type corresponding to TypeDefOrRef coded index.
   *
   * @param index
   * - TypeDefOrRef coded index according to 23.2.6.
   */
  def getTypeDefOrRef(index: Int): Type = {
    val tableId: Int = Table.tableId(Table._TypeDefOrRef, index)
    val row: Int = index >> Table.NoBits(Table._TypeDefOrRef)
    var `type`: Type = null
    tableId match {
      case Table.TypeDef.ID =>
        `type` = getTypeDef(row)
      case Table.TypeRef.ID =>
        return getTypeRef(row)
      case Table.TypeSpec.ID =>
        val ts: Table.TypeSpec = pefile.TypeSpec
        ts.readRow(row)
        val posInBlobStream: Int = ts.Signature
        val blobArrWithLengthStripped: Array[Byte] = pefile.Blob.getBlob(posInBlobStream)
        val compressedUInt: Array[Byte] = compressUInt(blobArrWithLengthStripped.length)
        val byteArr: Array[Byte] = new Array[Byte](blobArrWithLengthStripped.length + compressedUInt.length)
        System.arraycopy(compressedUInt, 0, byteArr, 0, compressedUInt.length)
        System.arraycopy(blobArrWithLengthStripped, 0, byteArr, compressedUInt.length, blobArrWithLengthStripped.length)
        val buf: ByteBuffer = ByteBuffer.wrap(byteArr)
        val sig: PEFile#Sig = new pefile.Sig(buf)
        val desc: Int = sig.readByte
        desc match {
          case Signature.ELEMENT_TYPE_GENERICINST =>
            val b: Int = sig.readByte
            val instantiatedType: Type = getTypeDefOrRef(sig.decodeInt)
            val numberOfTypeArgs: Int = sig.decodeInt
            val typeArgs: Array[Type] = new Array[Type](numberOfTypeArgs)

          {
            var iarg: Int = 0
            while (iarg < numberOfTypeArgs) {
              {
                typeArgs(iarg) = sig.decodeType
              }
              ({
                iarg += 1; iarg - 1
              })
            }
          }
            `type` = new ConstructedType(instantiatedType, typeArgs)
          case Signature.ELEMENT_TYPE_VAR =>
            val typeArgAsZeroBased: Int = sig.decodeInt
            `type` = new Type.TMVarUsage(typeArgAsZeroBased, true)
          case Signature.ELEMENT_TYPE_MVAR =>
            val typeArgAsZeroBased: Int = sig.decodeInt
            `type` = new Type.TMVarUsage(typeArgAsZeroBased, false)
          case Signature.ELEMENT_TYPE_SZARRAY =>
            sig.skipCustomMods
            `type` = Type.mkArray(sig.decodeType, 1)
          case Signature.ELEMENT_TYPE_ARRAY =>
            val elem: Type = sig.decodeType
            val rank: Int = sig.decodeInt
            val numSizes: Int = sig.decodeInt

          {
            var i: Int = 0
            while (i < numSizes) {
              sig.decodeInt
              ({
                i += 1; i - 1
              })
            }
          }
            val numLoBounds: Int = sig.decodeInt

          {
            var i: Int = 0
            while (i < numLoBounds) {
              sig.decodeInt
              ({
                i += 1; i - 1
              })
            }
          }
            `type` = Type.mkArray(elem, rank)
          case _ =>
            throw new RuntimeException("PEModule.getTypeDefOrRef(): TypeSpec")
        }
      case _ =>
        throw new RuntimeException("PEModule.getTypeDefOrRef(): oops!")
    }
    `type`
  }

  private def compressUInt(u: Int): Array[Byte] = {
    if (u <= 127 && 0 <= u) {
      return Array[Byte](u.toByte)
    }
    else if (u > 127 && u <= (2 ^ 14 - 1)) {
      val loByte: Byte = (u & 0xff).toByte
      val hiByte: Byte = ((u >> 8) | 0x80).toByte
      val res: Array[Byte] = Array[Byte](hiByte, loByte)
      return res
    }
    else {
      val b0: Byte = (u & 0xff).toByte
      val b1: Byte = ((u & 0xff00) >> 8).toByte
      val b2: Byte = ((u & 0xff0000) >> 16).toByte
      val b3: Byte = ((u >> 24) | 0xc0).toByte
      val res: Array[Byte] = Array[Byte](b3, b2, b1, b0)
      return res
    }
  }

  /**
   * Returns the method defined at the given row of the MethodDef table by
   * looking up the type that defines the method.
   */
  private[cilly] def getMethod(row: Int): MethodBase = {
    {
      var i: Int = 0
      while (i < types.length) {
          val `type`: PEType = types(i).asInstanceOf[PEType]
          if ((`type`.methodListBeg <= row) && (row < `type`.methodListEnd)) {
            `type`.doInitMethods()
            return `type`.methoddefs(row - `type`.methodListBeg)
          }
          i += 1
      }
    }
    throw new RuntimeException("In module " + this + ": cannot find type defining method 0x" + PEFile.int2hex(row))
  }

  /**
   * Returns the member referenced by the given row of the MemberRef table.
   */
  protected def getMemberRef(row: Int): MemberInfo = {
    return getMemberRef(row, null)
  }

  /**
   * Returns the member referenced by the given row of the MemberRef table if
   * defined in the given assembly. <i>Used by initCustomAttributes to avoid
   * unnecessary loading of referenced assemblies</i>
   */
  protected def getMemberRef(row: Int, inAssembly: Assembly): MemberInfo = {
    var member: MemberInfo = null
    val mref: Table.MemberRef = pefile.MemberRef
    mref.readRow(row)
    val mtbl: Int = Table.tableId(Table._MemberRefParent, mref.Class)
    val mind: Int = Table.tableIndex(Table._MemberRefParent, mref.Class)
    mtbl match {
      case TypeRef.ID =>
        val `type`: Type = getTypeRef(mind, inAssembly)
        if (`type` == null) return null
        val sig: PEFile#Sig = mref.getSignature
        val callconv: Int = sig.readByte
        val paramCount: Int = sig.decodeInt
        val retType: Type = sig.decodeRetType
        val paramType: Array[Type] = new Array[Type](paramCount)

      {
        var i: Int = 0
        while (i < paramCount) {
          paramType(i) = sig.decodeParamType
          ({
            i += 1; i - 1
          })
        }
      }
        val memberName: String = mref.getName
        if ((memberName == ConstructorInfo.CTOR) || (memberName == ConstructorInfo.CCTOR)) {
          member = `type`.getConstructor(paramType)
        }
        else {
          member = `type`.getMethod(memberName, paramType)
        }
        assert(member != null, `type` + "::" + memberName)
      case ModuleRef.ID =>
      case MethodDef.ID =>
      case TypeSpec.ID =>
        throw new RuntimeException("initCustomAttributes: " + pefile.getTable(mtbl).getTableName)
    }
    return member
  }

  protected def initCustomAttributes(attributeType: Type): Unit = {
    initAttributes(this, definingRow, Table.ModuleDef.ID, attributeType)
  }

  private[cilly] def initAttributes(cap: CustomAttributeProvider, definingRow: Int, sourceTableId: Int, attributeType: Type): Unit = {
    val parentIndex: Int = Table.encodeIndex(definingRow, Table._HasCustomAttribute, sourceTableId)
    val attrs: Table.CustomAttribute = pefile.CustomAttribute

    {
      var row: Int = 1
      while (row <= attrs.rows) {
        {
          var attrConstr: ConstructorInfo = null
          attrs.readRow(row)
          if (attrs.Parent == parentIndex) {
            val tableId: Int = Table.tableId(Table._CustomAttributeType, attrs.Type)
            val ind: Int = Table.tableIndex(Table._CustomAttributeType, attrs.Type)
            tableId match {
              case MethodDef.ID =>
                attrConstr = this.getMethod(ind).asInstanceOf[ConstructorInfo]
              case MemberRef.ID =>
                val attrAssem: Assembly = if (attributeType == null) null else attributeType.assembly
                val mi: MemberInfo = this.getMemberRef(ind, attrAssem)
                if (mi != null) {
                  assert(mi.isInstanceOf[ConstructorInfo], "Expected ConstructorInfo; found " + mi)
                  attrConstr = mi.asInstanceOf[ConstructorInfo]
                }
              case _ =>
                throw new RuntimeException
            }
            if (attrConstr != null && ((attrConstr.declaringType eq attributeType) || attributeType == null)) cap.addCustomAttribute(attrConstr, attrs.getValue)
          }
        }
        ({
          row += 1; row - 1
        })
      }
    }
  }
}