/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.util.ArrayList

import cilly.util.Signature._
import cilly.util.{PECustomMod, Signature, Table}

/**
 * Represents a type from a .NET assembly
 *
 * @param file        The PEFile that holds the description of the type.
 * @param definingRow The index in the TypeDef table where the type description is.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final class PEType(override val module: PEModule, override val attributes: Int, fullName: String, override val declaringType: Type, override val auxAttr: Int, val file: PEFile, val definingRow: Int) extends Type(module, attributes, fullName, null, null, declaringType, auxAttr) {

  /** The row of the first method in the MethodDef table. */
  private[cilly] final val methodListBeg: Int = file.typeDef(definingRow).MethodList
  /** The row of the last method in the MethodDef table + 1. */
  private[cilly] final val methodListEnd: Int = if (definingRow < file.typeDef.rows) file.typeDef(definingRow + 1).MethodList else file.methodDef.rows + 1


  protected override def loadBaseType(): Unit = {
    val typ: Table.TypeDef = file.typeDef(definingRow)
    baseType = if (typ.Extends == 0) null else (module.asInstanceOf[PEModule]).getTypeDefOrRef(typ.Extends)
  }

  protected override def loadFields(): Unit = {
    val fields: ArrayList[FieldInfo] = new ArrayList[FieldInfo]
    val fieldListBeg: Int = file.typeDef(definingRow).FieldList
    var fieldListEnd: Int = file.fieldDef.rows + 1
    if (definingRow < file.typeDef.rows) fieldListEnd = file.typeDef(definingRow + 1).FieldList

    {
      var row: Int = fieldListBeg
      while (row < fieldListEnd) {
        {
          val frow: Int = if (file.fieldTrans.rows == 0) row else file.fieldTrans(row).Field
          val attrs: Int = file.fieldDef(frow).Flags
          val name: String = file.fieldDef.getName
          val sig: PEFile#Sig = file.fieldDef.getSignature
          val pecmod: PECustomMod = sig.decodeFieldType
          var `val`: AnyRef = null
          val consts: Table.Constant = file.Constant

          {
            var i: Int = 1
            while (i <= consts.rows) {
              {
                consts.readRow(i)
                val tableId: Int = Table.tableId(Table._HasConstant, consts.Parent)
                val refRow: Int = consts.Parent >> Table.NoBits(Table._HasConstant)
                if (tableId == Table.FieldDef.ID && refRow == frow) `val` = consts.getValue
              }
              ({
                i += 1; i - 1
              })
            }
          }
          val field: FieldInfo = new PEFieldInfo(row, name, attrs.toShort, pecmod, `val`)
          if ((field.name == "value__") && field.isSpecialName) {
            assert(underlyingType == null, underlyingType.toString)
            underlyingType = field.fieldType
          }
          fields.add(field)
        }
        ({
          row += 1; row - 1
        })
      }
    }
    this.fields = fields.toArray(Array.empty)
    fields.clear()
  }

  private[cilly] var methoddefs: Array[MethodBase] = null

  protected def getMethod(n: Int): MethodInfo = {
    return methoddefs(n - methodListBeg).asInstanceOf[MethodInfo]
  }

  protected override def loadMethods(): Unit = {
    methoddefs = new Array[MethodBase](methodListEnd - methodListBeg)
    val methods: ArrayList[MethodInfo] = new ArrayList[MethodInfo]
    val constrs: ArrayList[ConstructorInfo] = new ArrayList[ConstructorInfo]
    val pemodule: PEModule = module.asInstanceOf[PEModule]
    
    {
      var row: Int = methodListBeg
      while (row < methodListEnd) {
        {
          val mrow: Int = if (file.methodTrans.rows == 0) row else file.methodTrans(row).Method
          val attrs: Int = file.methodDef(mrow).Flags
          val name: String = file.methodDef.getName
          val sig: PEFile#Sig = file.methodDef.getSignature
          val callConv: Int = sig.readByte
          if ((callConv & 0x1F) == Signature.GENERIC) {
            val genParamCount: Int = sig.decodeInt
          }
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
          val params: Array[ParameterInfo] = new Array[ParameterInfo](paramCount)
          val paramListBeg: Int = file.methodDef.ParamList
          var paramListEnd: Int = paramListBeg + paramCount
          if (paramListEnd > file.paramDef.rows) {
            paramListEnd = file.paramDef.rows + 1
          }
          {
            var i: Int = paramListBeg
            while (i < paramListEnd) {
              {
                val pattr: Int = file.paramDef(i).Flags
                val paramName: String = file.paramDef.getName
                val seq: Int = file.paramDef.Sequence
                if (seq == 0) {
                }
                else {
                  params(seq - 1) = new ParameterInfo(paramName, paramType(seq - 1), pattr.toShort, seq - 1)
                }
              }
              ({
                i += 1; i - 1
              })
            }
          }
          {
            var i: Int = 0
            while (i < params.length) {
              {
                if (params(i) == null) params(i) = new ParameterInfo(null, paramType(i), 0, 0)
              }
              ({
                i += 1; i - 1
              })
            }
          }
          if (((attrs & MethodAttributes.SpecialName) != 0) && ((attrs & MethodAttributes.RTSpecialName) != 0) && ((name == ConstructorInfo.CTOR) || (name == ConstructorInfo.CCTOR))) {
            val constr: ConstructorInfo = new PEConstructorInfo(row, attrs.toShort, params)
            constrs.add(constr)
            methoddefs(row - methodListBeg) = constr
          }
          else {
            val method: MethodInfo = new PEMethodInfo(row, name, attrs.toShort, retType, params)
            val mvarIdxes: Array[Int] = file.genericParam.getMVarIdxes(row)

            {
              var i: Int = 0
              while (i < mvarIdxes.length) {
                {
                  val mvarAndConstraints: GenericParamAndConstraints = pemodule.getTypeConstraints(mvarIdxes(i))
                  method.addMVar(mvarAndConstraints)
                }
                ({
                  i += 1; i - 1
                })
              }
            }
            methods.add(method)
            methoddefs(row - methodListBeg) = method
          }
        }
        ({
          row += 1; row - 1
        })
      }
    }
    this.constructors = constrs.toArray(ConstructorInfo.EMPTY_ARRAY)
    this.methods = methods.toArray(Array.empty)
    constrs.clear()
    methods.clear()
  }

  protected override def loadProperties(): Unit = {
    val pmap: Table.PropertyMap = file.PropertyMap
    if (pmap == null) {
      this.properties = Array.empty
      return
    }
    val pdef: Table.PropertyDef = file.PropertyDef
    var propListBeg: Int = -1
    var propListEnd: Int = pdef.rows + 1
    def loop(): Unit = {
      {
        var i: Int = 1
        while (i <= pmap.rows) {
          {
            pmap.readRow(i)
            if (pmap.Parent == this.definingRow) {
              propListBeg = pmap.PropertyList
              if (i < pmap.rows) {
                pmap.readRow(i + 1)
                propListEnd = pmap.PropertyList
              }
              return
            }
          }
          i += 1
        }
      }
    }
    loop()
    if (propListBeg < 0) {
      this.properties = Array.empty
      return
    }
    val properties: ArrayList[PropertyInfo] = new ArrayList[PropertyInfo]

    {
      var i: Int = propListBeg
      while (i < propListEnd) {
          pdef.readRow(i)
          val sig: PEFile#Sig = pdef.getSignature
          var b: Int = sig.readByte
          b &= ~HASTHIS
          val paramCount: Int = sig.readByte
          assert(b == PROPERTY)
          val propType: Type = sig.decodeType
          val index: Int = Table.encodeIndex(i, Table._HasSemantics, Table.PropertyDef.ID)
          val msem: Table.MethodSemantics = file.MethodSemantics
          var getter: MethodInfo = null
          var setter: MethodInfo = null

          {
            var j: Int = 1
            while (j <= msem.rows) {
                msem.readRow(j)
                if (msem.Association != index) {
                  if (msem.isGetter) getter = getMethod(msem.Method)
                  else if (msem.isSetter) setter = getMethod(msem.Method)
                  else System.err.println("PEType.loadProperties(): !?!")
                }
                j += 1
            }
          }
          properties.add(new PEPropertyInfo(i, pdef.getName, pdef.Flags.toShort, propType, getter, setter))
          i += 1
      }
    }
    this.properties = properties.toArray(Array.empty)
  }

  protected override def loadEvents(): Unit = {
    val emap: Table.EventMap = file.EventMap
    if (emap == null) {
      this.events = EventInfo.EMPTY_ARRAY
      return
    }
    val edef: Table.EventDef = file.EventDef
    var eventListBeg: Int = -1
    var eventListEnd: Int = edef.rows + 1

    def loop(): Unit = {
      {
        var i: Int = 1
        while (i <= emap.rows) {
          {
            emap.readRow(i)
            if (emap.Parent == this.definingRow) {
              eventListBeg = emap.EventList
              if (i < emap.rows) {
                emap.readRow(i + 1)
                eventListEnd = emap.EventList
              }
              return
            }
          }
          ({
            i += 1;
            i - 1
          })
        }
      }
    }
    if (eventListBeg < 0) {
      this.events = EventInfo.EMPTY_ARRAY
      return
    }
    val events: ArrayList[PEType#PEEventInfo] = new ArrayList[PEType#PEEventInfo]
    val msem: Table.MethodSemantics = file.MethodSemantics

    {
      var i: Int = eventListBeg
      while (i < eventListEnd) {
        {
          edef.readRow(i)
          val handler: Type = (module.asInstanceOf[PEModule]).getTypeDefOrRef(edef.EventType)
          val index: Int = Table.encodeIndex(i, Table._HasSemantics, Table.EventDef.ID)
          var add: MethodInfo = null
          var remove: MethodInfo = null

          {
            var j: Int = 1
            while (j <= msem.rows) {
                msem.readRow(j)
                if (msem.Association != index) {
                  if (msem.isAddOn) add = getMethod(msem.Method)
                  else if (msem.isRemoveOn) remove = getMethod(msem.Method)
                  else {
                  }
                }
              j += 1
            }
          }
          events.add(new PEEventInfo(i, edef.getName, edef.EventFlags.toShort, handler, add, remove))
        }
        i += 1
      }
    }
    this.events = events.toArray(EventInfo.EMPTY_ARRAY)
  }

  protected override def loadNestedTypes(): Unit = {
    val nested: ArrayList[Type] = new ArrayList[Type]

    {
      var i: Int = 1
      while (i <= file.NestedClass.rows) {
        {
          file.NestedClass.readRow(i)
          if (file.NestedClass.enclosingClass == this.definingRow) nested.add((module.asInstanceOf[PEModule]).getTypeDef(file.NestedClass.nestedClass))
        }
        ({
          i += 1; i - 1
        })
      }
    }
    this.nestedTypes = nested.toArray(Type.EmptyTypes)
  }

  protected override def loadInterfaces(): Unit = {
    interfaces = Type.EmptyTypes
    val index: Int = file.InterfaceImpl.findType(definingRow)
    if (index > 0) {
      val ifaces: ArrayList[Type] = new ArrayList[Type]
      def loop(): Unit = {
        {
          var i: Int = index
          while (i <= file.InterfaceImpl.rows) {
            {
              file.InterfaceImpl.readRow(i)
              if (file.InterfaceImpl.Class != definingRow) return
              ifaces.add((module.asInstanceOf[PEModule]).getTypeDefOrRef(file.InterfaceImpl.Interface))
            }
            ({
              i += 1;
              i - 1
            })
          }
        }
      }
      interfaces = ifaces.toArray(new Array[Type](ifaces.size))
    }
  }

  protected override def loadCustomAttributes(attributeType: Type): Unit = {
    initAttributes(this, definingRow, Table.TypeDef.ID, attributeType)
  }

  private def initAttributes(cap: CustomAttributeProvider, definingRow: Int, sourceTableId: Int, attributeType: Type): Unit = {
    (this.module.asInstanceOf[PEModule]).initAttributes(cap, definingRow, sourceTableId, attributeType)
  }

  private class PEFieldInfo(val definingRow: Int, override val name: String, override val attributes: Short, override val fieldTypeWithMods: PECustomMod, override val value: AnyRef) extends FieldInfo(name, PEType.this, attributes, fieldTypeWithMods, value) {

    protected override def loadCustomAttributes(attributeType: Type): Unit = {
      PEType.this.initAttributes(this, definingRow, Table.FieldDef.ID, attributeType)
    }
  }

  private class PEMethodInfo(val definingRow: Int, override val name: String, override val attributes: Short, override val returnType: Type, override val params: Array[ParameterInfo]) extends MethodInfo(name, PEType.this, attributes, returnType, params) {
    protected override def loadCustomAttributes(attributeType: Type): Unit = PEType.this.initAttributes(this, definingRow, Table.MethodDef.ID, attributeType)
  }

  private class PEConstructorInfo(val definingRow: Int, override val attributes: Short, override val params: Array[ParameterInfo]) extends ConstructorInfo(PEType.this, attributes, params) {
    protected override def loadCustomAttributes(attributeType: Type): Unit = PEType.this.initAttributes(this, definingRow, Table.MethodDef.ID, attributeType)
  }

  private class PEPropertyInfo(val definingRow: Int, override val name: String, override val attributes: Short, override val declaringType: Type, override val getter: MethodInfo, override val setter: MethodInfo) extends PropertyInfo(name, PEType.this, attributes, declaringType, getter, setter) {
    protected override def loadCustomAttributes(attributeType: Type): Unit = PEType.this.initAttributes(this, definingRow, Table.PropertyDef.ID, attributeType)
  }

  private class PEEventInfo(val definingRow: Int, override val name: String, override val attributes: Short, override val declaringType: Type, override val addMethod: MethodInfo, override val removeMethod: MethodInfo) extends EventInfo(name, PEType.this, attributes, declaringType, addMethod, removeMethod) {
    protected override def loadCustomAttributes(attributeType: Type): Unit = PEType.this.initAttributes(this, definingRow, Table.EventDef.ID, attributeType)
  }

}