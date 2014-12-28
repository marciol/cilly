/*
 * System.Reflection-like API for acces to .NET Assemblies
 */
package cilly.util

import java.nio.{ ByteBuffer, MappedByteBuffer }
import java.util
import java.util.{ Collections, HashMap, HashSet, Iterator, Map, Set }

import cilly.PEFile

/** Represents a table in a .NET assembly
  *
  * @author Nikolay Mihaylov
  * @version 1.0
  */
object Table {
  val MAX_NUMBER: Int = 64
  val VALID_TABLES_MASK: Long = 0x03ff3fb7ff57L
  val TABLE_SET_LENGTH: Int = 13
  val _TypeDefOrRef: Int = 0
  val _HasConstant: Int = 1
  val _HasCustomAttribute: Int = 2
  val _HasFieldMarshal: Int = 3
  val _HasDeclSecurity: Int = 4
  val _MemberRefParent: Int = 5
  val _HasSemantics: Int = 6
  val _MethodDefOrRef: Int = 7
  val _MemberForwarded: Int = 8
  val _Implementation: Int = 9
  val _CustomAttributeType: Int = 10
  val _ResolutionScope: Int = 11
  val _TypeOrMethodDef: Int = 12
  val TableSet: Array[Array[Int]] = Array(
    Array[Int](TypeDef.ID, TypeRef.ID, TypeSpec.ID),
    Array[Int](FieldDef.ID, ParamDef.ID, PropertyDef.ID),
    Array[Int](MethodDef.ID, FieldDef.ID, TypeRef.ID, TypeDef.ID, ParamDef.ID, InterfaceImpl.ID, MemberRef.ID, ModuleDef.ID, -1, PropertyDef.ID, EventDef.ID, -1, ModuleRef.ID, TypeSpec.ID, AssemblyDef.ID, AssemblyRef.ID, FileDef.ID, ExportedType.ID, ManifestResource.ID),
    Array[Int](FieldDef.ID, ParamDef.ID),
    Array[Int](TypeDef.ID, MethodDef.ID, AssemblyDef.ID),
    Array[Int](-1, TypeRef.ID, ModuleRef.ID, MethodDef.ID, TypeSpec.ID),
    Array[Int](EventDef.ID, PropertyDef.ID),
    Array[Int](MethodDef.ID, MemberRef.ID),
    Array[Int](FieldDef.ID, MethodDef.ID),
    Array[Int](FileDef.ID, AssemblyRef.ID, ExportedType.ID),
    Array[Int](-1, -1, MethodDef.ID, MemberRef.ID, -1),
    Array[Int](ModuleDef.ID, ModuleRef.ID, AssemblyRef.ID, TypeRef.ID),
    Array[Int](TypeDef.ID, MethodDef.ID))

  val NoBits: Array[Int] = Array[Int](2, 2, 5, 1, 2, 3, 1, 1, 1, 2, 3, 2, 1)

  def mask(tableSetId: Int): Int = (1 << NoBits(tableSetId)) - 1

  def tableId(tableSet: Int, index: Int): Int = TableSet(tableSet)(index & mask(tableSet))

  def tableIndex(tableSet: Int, index: Int): Int = index >> NoBits(tableSet)

  def encodeIndex(index: Int, tableSetId: Int, tableId: Int): Int = {
    val tableSet: Array[Int] = TableSet(tableSetId)

    var i: Int = 0
    while (i < tableSet.length) {
      if (tableSet(i) == tableId) return (index << NoBits(tableSetId)) | i
      i += 1
    }
    throw new RuntimeException("Cannot find table #" + tableId + " in table set #" + tableSetId)
  }

  private val tableName: Array[String] = Array("Module", "TypeRef", "TypeDef", "   FieldTrans", "Field", "MethodTrans", "Method", "", "Param", "InterfaceImpl", "MemberRef", "Constant", "CustomAttribute", "FieldMarshal", "DeclSecurity", "ClassLayout", "FieldLayout", "StandAloneSig", "EventMap", "", "Event", "PropertyMap", "", "Property", "MethodSemantics", "MethodImpl", "ModuleRef", "TypeSpec", "ImplMap", "FieldRVA", "", "", "Assembly", "AssemblyProcessor", "AssemblyOS", "AssemblyRef", "AssemblyRefProcessor", "AssemblyRefOS", "File", "ExportedType", "ManifestResource", "NestedClass", "GenericParam", "MethodSpec", "GenericParamConstraint", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "")

  /** Creates a table with the given id and number of rows.
    */
  def newTable(file: PEFile, id: Int, rows: Int): Table =
    id match {
      case ModuleDef.ID =>
        new ModuleDef(file, rows)
      case TypeRef.ID =>
        new TypeRef(file, rows)
      case TypeDef.ID =>
        new TypeDef(file, rows)
      case FieldTrans.ID =>
        new FieldTrans(file, rows)
      case FieldDef.ID =>
        new FieldDef(file, rows)
      case MethodTrans.ID =>
        new MethodTrans(file, rows)
      case MethodDef.ID =>
        new MethodDef(file, rows)
      case ParamDef.ID =>
        new ParamDef(file, rows)
      case InterfaceImpl.ID =>
        new InterfaceImpl(file, rows)
      case MemberRef.ID =>
        new MemberRef(file, rows)
      case Constant.ID =>
        new Constant(file, rows)
      case CustomAttribute.ID =>
        new CustomAttribute(file, rows)
      case FieldMarshal.ID =>
        new FieldMarshal(file, rows)
      case DeclSecurity.ID =>
        new DeclSecurity(file, rows)
      case ClassLayout.ID =>
        new ClassLayout(file, rows)
      case FieldLayout.ID =>
        new FieldLayout(file, rows)
      case StandAloneSig.ID =>
        new StandAloneSig(file, rows)
      case EventMap.ID =>
        new EventMap(file, rows)
      case EventDef.ID =>
        new EventDef(file, rows)
      case PropertyMap.ID =>
        new PropertyMap(file, rows)
      case PropertyDef.ID =>
        new PropertyDef(file, rows)
      case MethodSemantics.ID =>
        new MethodSemantics(file, rows)
      case MethodImpl.ID =>
        new MethodImpl(file, rows)
      case ModuleRef.ID =>
        new ModuleRef(file, rows)
      case TypeSpec.ID =>
        new TypeSpec(file, rows)
      case ImplMap.ID =>
        new ImplMap(file, rows)
      case FieldRVA.ID =>
        new FieldRVA(file, rows)
      case AssemblyDef.ID =>
        new AssemblyDef(file, rows)
      case AssemblyProcessor.ID =>
        new AssemblyProcessor(file, rows)
      case AssemblyOS.ID =>
        new AssemblyOS(file, rows)
      case AssemblyRef.ID =>
        new AssemblyRef(file, rows)
      case AssemblyRefProcessor.ID =>
        new AssemblyRefProcessor(file, rows)
      case AssemblyRefOS.ID =>
        new AssemblyRefOS(file, rows)
      case FileDef.ID =>
        new FileDef(file, rows)
      case ExportedType.ID =>
        new ExportedType(file, rows)
      case ManifestResource.ID =>
        new ManifestResource(file, rows)
      case NestedClass.ID =>
        new NestedClass(file, rows)
      case GenericParam.ID =>
        new GenericParam(file, rows)
      case MethodSpec.ID =>
        new MethodSpec(file, rows)
      case GenericParamConstraint.ID =>
        new GenericParamConstraint(file, rows)
      case _ =>
        new Empty(id)
    }

  private final class Empty(override val id: Int) extends Table(null, id, 0) {

    protected def getRowSize: Int = 0

    protected def populateFields(): Unit =
      throw new RuntimeException("Table 0x" + PEFile.byte2hex(id))
  }

  object ModuleDef {
    val ID: Int = 0x00
  }

  final class ModuleDef(file: PEFile, rows: Int) extends Table(file, ModuleDef.ID, rows) {
    /** 2-byte value; reserved - shall be 0. */
    var Generation: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into #GUID; used to distinguish between two version of the same
      * module.
      */
    var Mvid: Int = 0
    /** Index into #GUID; reserved - shall be 0. */
    var EncId: Int = 0
    /** Index into #GUID; reseved - shall be 0. */
    var EncBaseId: Int = 0

    protected def populateFields(): Unit = {
      Generation = readShort
      Name = readStringIndex
      Mvid = readGUIDIndex
      EncId = readGUIDIndex
      EncBaseId = readGUIDIndex
    }

    protected def getRowSize: Int = 2 + file.getStringIndexSize + 3 * file.getGUIDIndexSize

    def name: String = file.getString(Name)
  }

  object TypeRef {
    val ID: Int = 0x1
  }

  final class TypeRef(file: PEFile, rows: Int) extends Table(file, TypeRef.ID, rows) {
    /** A ResolutionScope coded index. */
    var ResolutionScope: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into #String. */
    var Namespace: Int = 0

    protected def populateFields(): Unit = {
      ResolutionScope = readTableSetIndex(_ResolutionScope)
      Name = readStringIndex
      Namespace = readStringIndex
    }

    protected def getRowSize: Int = {
      return file.getTableSetIndexSize(_ResolutionScope) + 2 * file.getStringIndexSize
    }

    def getFullName: String = {
      val namespace: String = file.getString(Namespace)
      return if (namespace.length == 0) file.getString(Name) else namespace + "." + file.getString(Name)
    }
  }

  object TypeDef {
    val ID: Int = 0x02
  }

  final class TypeDef(file: PEFile, rows: Int) extends Table(file, TypeDef.ID, rows) {
    /** 4-byte bitmask of type TypeAttributes (22.1.14). */
    var Flags: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into #String. */
    var Namespace: Int = 0
    /** TypeDefOrRef coded index. */
    var Extends: Int = 0
    /** Index into Field table.
      */
    var FieldList: Int = 0
    /** Index into Method table. */
    var MethodList: Int = 0

    this.newMapping = true

    def getFullName: String = {
      val namespace: String = file.getString(Namespace)
      return if (namespace.length == 0) file.getString(Name) else namespace + "." + file.getString(Name)
    }

    protected def populateFields(): Unit = {
      Flags = readInt
      Name = readStringIndex
      Namespace = readStringIndex
      Extends = readTableSetIndex(_TypeDefOrRef)
      FieldList = readTableIndex(FieldDef.ID)
      MethodList = readTableIndex(MethodDef.ID)
    }

    protected def getRowSize: Int = {
      return 4 + 2 * file.getStringIndexSize + file.getTableSetIndexSize(_TypeDefOrRef) + file.getTableIndexSize(FieldDef.ID) + file.getTableIndexSize(MethodDef.ID)
    }
  }

  /** Undocumented table. Appears to be used for translating the Field entry in
    * the TypeDef(0x02) table into the real entry in the Fields(0x06) table
    */
  object FieldTrans {
    val ID: Int = 0x03
  }

  final class FieldTrans(file: PEFile, rows: Int) extends Table(file, FieldTrans.ID, rows) {
    var Field: Int = 0
    newMapping = true

    protected def populateFields(): Unit = {
      Field = readTableIndex(FieldDef.ID)
    }

    protected def getRowSize: Int = {
      return file.getTableIndexSize(FieldDef.ID)
    }
  }

  object FieldDef {
    val ID: Int = 0x04
  }

  final class FieldDef(file: PEFile, rows: Int) extends Table(file, FieldDef.ID, rows) {
    /** 2-byte bitmask of type FieldAttributes (22.1.5). */
    var Flags: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into #Blob. */
    var Signature: Int = 0

    newMapping = true

    protected def populateFields(): Unit = {
      Flags = readShort
      Name = readStringIndex
      Signature = readBlobIndex
    }

    protected def getRowSize: Int = {
      return 2 + file.getStringIndexSize + file.getBlobIndexSize
    }

    def getName: String = {
      return file.getString(Name)
    }

    def getSignature: PEFile#Sig = {
      return file.getSignature(Signature)
    }
  }

  /** Undocumented table. Appears to be used for translating the Method entry in
    * the TypeDef(0x02) table into the real entry in the Methods(0x06) table
    */
  object MethodTrans {
    val ID: Int = 0x05
  }

  final class MethodTrans(file: PEFile, rows: Int) extends Table(file, MethodTrans.ID, rows) {
    var Method: Int = 0

    newMapping = true

    protected def populateFields(): Unit = {
      Method = readTableIndex(FieldDef.ID)
    }

    protected def getRowSize: Int = {
      return file.getTableIndexSize(MethodDef.ID)
    }
  }

  object MethodDef {
    val ID: Int = 0x06
  }

  final class MethodDef(file: PEFile, rows: Int) extends Table(file, MethodDef.ID, rows) {
    /** 4-byte constant. */
    var RVA: Int = 0
    /** 2-byte bitmask of type MethodImplAttributes (22.1.10). */
    var ImplFlags: Int = 0
    /** 2-byte bitmask of type MethodAttributes (22.1.9). */
    var Flags: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into #Blob. */
    var Signature: Int = 0
    /** Index into Param Table. */
    var ParamList: Int = 0

    newMapping = true

    protected def populateFields(): Unit = {
      RVA = readInt
      ImplFlags = readShort
      Flags = readShort
      Name = readStringIndex
      Signature = readBlobIndex
      ParamList = readTableIndex(ParamDef.ID)
    }

    protected def getRowSize: Int = {
      return 8 + file.getStringIndexSize + file.getBlobIndexSize + file.getTableIndexSize(ParamDef.ID)
    }

    def getName: String = {
      return file.getString(Name)
    }

    def getSignature: PEFile#Sig = {
      return file.getSignature(Signature)
    }
  }

  object ParamDef {
    val ID: Int = 0x08
  }

  final class ParamDef(file: PEFile, rows: Int) extends Table(file, ParamDef.ID, rows) {
    /** 2-byte bitmask of type ParamAttributes (22.1.12). */
    var Flags: Int = 0
    /** 2-byte constant. */
    var Sequence: Int = 0
    /** Index into #String. */
    var Name: Int = 0

    newMapping = true

    protected def populateFields(): Unit = {
      Flags = readShort
      Sequence = readShort
      Name = readStringIndex
    }

    protected def getRowSize: Int = {
      return 4 + file.getStringIndexSize
    }

    def getName: String = {
      return file.getString(Name)
    }
  }

  object InterfaceImpl {
    val ID: Int = 0x09
  }

  final class InterfaceImpl(file: PEFile, rows: Int) extends Table(file, InterfaceImpl.ID, rows) {
    /** Index into TypeDef table. */
    var Class: Int = 0
    /** Index into TypeDefOrRef table set. */
    var Interface: Int = 0

    protected def populateFields(): Unit = {
      Class = readTableIndex(TypeDef.ID)
      Interface = readTableSetIndex(_TypeDefOrRef)
    }

    protected def getRowSize: Int = {
      return file.getTableIndexSize(TypeDef.ID) + file.getTableSetIndexSize(_TypeDefOrRef)
    }

    /** finds the index of the first entry
      *
      * @param targetIndex
      * - index in the TypeDef table - the type to look for
      * @return the index of the first interface for the given type; 0 if the
      *        type doesn't implement any interfaces
      */
    def findType(targetIndex: Int): Int = {
      {
        var i: Int = 1
        while (i <= rows) {
          {
            seekRow(i)
            if (targetIndex == readTableIndex(TypeDef.ID)) return i
          }
          ({
            i += 1;
            i - 1
          })
        }
      }
      return 0
    }
  }

  object MemberRef {
    val ID: Int = 0x0a
  }

  final class MemberRef(file: PEFile, rows: Int) extends Table(file, MemberRef.ID, rows) {
    /** Index into MemberRefParent table set. */
    var Class: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into #Blob. */
    var Signature: Int = 0

    protected def populateFields(): Unit = {
      Class = readTableSetIndex(_MemberRefParent)
      Name = readStringIndex
      Signature = readBlobIndex
    }

    protected def getRowSize: Int = {
      return file.getTableSetIndexSize(_MemberRefParent) + file.getStringIndexSize + file.getBlobIndexSize
    }

    def getName: String = {
      return file.getString(Name)
    }

    def getSignature: PEFile#Sig = {
      return file.getSignature(Signature)
    }
  }

  object Constant {
    val ID: Int = 0x0b
  }

  final class Constant(file: PEFile, rows: Int) extends Table(file, Constant.ID, rows) {
    /** 1-byte constant followed by 1-byte padding 0 (see 22.1.15). */
    var Type: Int = 0
    /** Index into HasConst table set. */
    var Parent: Int = 0
    /** Index into #Blob. */
    var Value: Int = 0

    protected def populateFields(): Unit = {
      Type = readShort
      Parent = readTableSetIndex(_HasConstant)
      Value = readBlobIndex
    }

    protected def getRowSize: Int = {
      return 2 + file.getTableSetIndexSize(_HasConstant) + file.getBlobIndexSize
    }

    def getValue: AnyRef = {
      if (Type == Signature.ELEMENT_TYPE_CLASS) return null
      return file.Blob.getConstant(Type, Value)
    }
  }

  object CustomAttribute {
    val ID: Int = 0x0c
  }

  final class CustomAttribute(file: PEFile, rows: Int) extends Table(file, CustomAttribute.ID, rows) {
    /** Index into any metadata table, except the CustomAttribute itself; more
      * precisely - index into HasCustomAttribute table set.
      */
    var Parent: Int = 0
    /** Index into the CustomAttributeType table set. */
    var Type: Int = 0
    /** Index into #Blob. */
    var Value: Int = 0

    protected def populateFields(): Unit = {
      Parent = readTableSetIndex(_HasCustomAttribute)
      Type = readTableSetIndex(_CustomAttributeType)
      Value = readBlobIndex
    }

    protected def getRowSize: Int = {
      return file.getTableSetIndexSize(_HasCustomAttribute) + file.getTableSetIndexSize(_CustomAttributeType) + file.getBlobIndexSize
    }

    def getValue: Array[Byte] = {
      return if (Value == 0) null else file.getBlob(Value)
    }
  }

  object FieldMarshal {
    val ID: Int = 0x0d
  }

  final class FieldMarshal(file: PEFile, rows: Int) extends Table(file, FieldMarshal.ID, rows) {
    /** Index into HasFieldMarshal table set. */
    var Parent: Int = 0
    /** Index into #Blob. */
    var NativeType: Int = 0

    protected def populateFields(): Unit = {
      Parent = readTableSetIndex(_HasFieldMarshal)
      NativeType = readBlobIndex
    }

    protected def getRowSize: Int = {
      return file.getTableSetIndexSize(_HasFieldMarshal) + file.getBlobIndexSize
    }
  }

  object DeclSecurity {
    val ID: Int = 0x0e
  }

  final class DeclSecurity(file: PEFile, rows: Int) extends Table(file, DeclSecurity.ID, rows) {
    /** 2-byte value. */
    var Action: Int = 0
    /** Index into HasDeclSecurity table set. */
    var Parent: Int = 0
    /** Index into #Blob. */
    var PermissionSet: Int = 0

    protected def populateFields(): Unit = {
      Action = readShort
      Parent = readTableSetIndex(_HasDeclSecurity)
      PermissionSet = readBlobIndex
    }

    protected def getRowSize: Int = {
      return 2 + file.getTableSetIndexSize(_HasDeclSecurity) + file.getBlobIndexSize
    }
  }

  object ClassLayout {
    val ID: Int = 0x0f
  }

  final class ClassLayout(file: PEFile, rows: Int) extends Table(file, ClassLayout.ID, rows) {
    /** 2-byte constant. */
    var PackingSize: Int = 0
    /** 4-byte constant. */
    var ClassSize: Int = 0
    /** Index into TypeDef table. */
    var Parent: Int = 0

    protected def populateFields(): Unit = {
      PackingSize = readShort
      ClassSize = readInt
      Parent = readTableIndex(TypeDef.ID)
    }

    protected def getRowSize: Int = {
      return 6 + file.getTableIndexSize(TypeDef.ID)
    }
  }

  object FieldLayout {
    val ID: Int = 0x10
  }

  final class FieldLayout(file: PEFile, rows: Int) extends Table(file, FieldLayout.ID, rows) {
    /** 4-byte constant. */
    var Offset: Int = 0
    /** Index into the Field table. */
    var Field: Int = 0

    protected def populateFields(): Unit = {
      Offset = readInt
      Field = readTableIndex(FieldDef.ID)
    }

    protected def getRowSize: Int = {
      return 4 + file.getTableIndexSize(FieldDef.ID)
    }
  }

  object StandAloneSig {
    val ID: Int = 0x11
  }

  final class StandAloneSig(file: PEFile, rows: Int) extends Table(file, StandAloneSig.ID, rows) {
    /** Index into #Blob. */
    var Signature: Int = 0

    protected def populateFields(): Unit = {
      Signature = readBlobIndex
    }

    protected def getRowSize: Int = {
      return file.getBlobIndexSize
    }
  }

  object EventMap {
    val ID: Int = 0x12
  }

  final class EventMap(file: PEFile, rows: Int) extends Table(file, EventMap.ID, rows) {
    /** Index into the TypeDef table. */
    var Parent: Int = 0
    /** Index into the Event table. */
    var EventList: Int = 0

    protected def populateFields(): Unit = {
      Parent = readTableIndex(TypeDef.ID)
      EventList = readTableIndex(EventDef.ID)
    }

    protected def getRowSize: Int = {
      return file.getTableIndexSize(TypeDef.ID) + file.getTableIndexSize(EventDef.ID)
    }
  }

  object EventDef {
    val ID: Int = 0x14
  }

  final class EventDef(file: PEFile, rows: Int) extends Table(file, EventDef.ID, rows) {
    /** 2-byte bitmask of type EventAttribute (22.1.4). */
    var EventFlags: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into TypeDefOrRef table set. [This corresponds to the Type of the
      * event; it is not the Type that owns the event]
      */
    var EventType: Int = 0

    protected def populateFields(): Unit = {
      EventFlags = readShort
      Name = readStringIndex
      EventType = readTableSetIndex(_TypeDefOrRef)
    }

    protected def getRowSize: Int = {
      return 2 + file.getStringIndexSize + file.getTableSetIndexSize(_TypeDefOrRef)
    }

    def getName: String = {
      return file.getString(Name)
    }
  }

  object PropertyMap {
    val ID: Int = 0x15
  }

  final class PropertyMap(file: PEFile, rows: Int) extends Table(file, PropertyMap.ID, rows) {
    /** Index into the TypeDef table. */
    var Parent: Int = 0
    /** Index into the Property table. */
    var PropertyList: Int = 0

    protected def populateFields(): Unit = {
      Parent = readTableIndex(TypeDef.ID)
      PropertyList = readTableIndex(PropertyDef.ID)
    }

    protected def getRowSize: Int = {
      return file.getTableIndexSize(TypeDef.ID) + file.getTableIndexSize(PropertyDef.ID)
    }
  }

  object PropertyDef {
    val ID: Int = 0x17
  }

  final class PropertyDef(file: PEFile, rows: Int) extends Table(file, PropertyDef.ID, rows) {
    /** 2-byte bitmask of type PropertyAttributes (22.1.13). */
    var Flags: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into #Blob. (Indexes the signature in the #Blob) */
    var Type: Int = 0

    protected def populateFields(): Unit = {
      Flags = readShort
      Name = readStringIndex
      Type = readBlobIndex
    }

    protected def getRowSize: Int = {
      return 2 + file.getStringIndexSize + file.getBlobIndexSize
    }

    def getName: String = {
      return file.getString(Name)
    }

    def getSignature: PEFile#Sig = {
      return file.getSignature(Type)
    }
  }

  object MethodSemantics {
    val ID: Int = 0x18
    private val Setter: Short = 0x0001.toShort
    private val Getter: Short = 0x0002.toShort
    private val Other: Short = 0x0004.toShort
    private val AddOn: Short = 0x0008.toShort
    private val RemoveOn: Short = 0x0010.toShort
    private val Fire: Short = 0x0020.toShort
  }

  final class MethodSemantics(file: PEFile, rows: Int) extends Table(file, MethodSemantics.ID, rows) {
    /** 2-byte bitmaks of type MethodSemanticsAttribute (22.1.11). */
    var Semantics: Int = 0
    /** Index into the Method table. */
    var Method: Int = 0
    /** Index into Event or Property table (HasSemantics table set). */
    var Association: Int = 0

    protected def populateFields(): Unit = {
      Semantics = readShort
      Method = readTableIndex(MethodDef.ID)
      Association = readTableSetIndex(_HasSemantics)
    }

    protected def getRowSize: Int = {
      return 2 + file.getTableIndexSize(MethodDef.ID) + file.getTableSetIndexSize(_HasSemantics)
    }

    def isGetter: Boolean = {
      return (Semantics & MethodSemantics.Getter) != 0
    }

    def isSetter: Boolean = {
      return (Semantics & MethodSemantics.Setter) != 0
    }

    def isOther: Boolean = {
      return (Semantics & MethodSemantics.Other) != 0
    }

    def isAddOn: Boolean = {
      return (Semantics & MethodSemantics.AddOn) != 0
    }

    def isRemoveOn: Boolean = {
      return (Semantics & MethodSemantics.RemoveOn) != 0
    }

    def isFire: Boolean = {
      return (Semantics & MethodSemantics.Fire) != 0
    }
  }

  object MethodImpl {
    val ID: Int = 0x19
  }

  final class MethodImpl(file: PEFile, rows: Int) extends Table(file, MethodImpl.ID, rows) {
    /** Index into the TypeDef table. */
    var Class: Int = 0
    /** Index into MethodDefOrRef table set. */
    var MethodBody: Int = 0
    /** Index into MethodDefOrRef table set. */
    var MethodDeclaration: Int = 0

    protected def populateFields(): Unit = {
      Class = readTableIndex(TypeDef.ID)
      MethodBody = readTableSetIndex(_MethodDefOrRef)
      MethodDeclaration = readTableSetIndex(_MethodDefOrRef)
    }

    protected def getRowSize: Int = {
      return file.getTableIndexSize(TypeDef.ID) + 2 * file.getTableSetIndexSize(_MethodDefOrRef)
    }
  }

  object ModuleRef {
    val ID: Int = 0x1a
  }

  final class ModuleRef(file: PEFile, rows: Int) extends Table(file, ModuleRef.ID, rows) {
    /** Index into #String. */
    var Name: Int = 0

    protected def populateFields(): Unit = {
      Name = readStringIndex
    }

    protected def getRowSize: Int = {
      return file.getStringIndexSize
    }

    def getName: String = {
      return file.getString(Name)
    }
  }

  object TypeSpec {
    val ID: Int = 0x1b
  }

  final class TypeSpec(file: PEFile, rows: Int) extends Table(file, TypeSpec.ID, rows) {
    /** Index into #Blob, where the blob is formatted as specified in 22.2.15
      */
    var Signature: Int = 0

    protected def populateFields(): Unit = {
      Signature = readBlobIndex
    }

    protected def getRowSize: Int = {
      return file.getBlobIndexSize
    }

    def getSignature: PEFile#Sig = {
      return file.getSignature(Signature)
    }
  }

  object ImplMap {
    val ID: Int = 0x1c
  }

  final class ImplMap(file: PEFile, rows: Int) extends Table(file, ImplMap.ID, rows) {
    /** 2-byte bitmask of type PInvokeAttributes (22.1.7). */
    var mappingFlags: Int = 0
    /** Index into MemberForwarded table set. */
    var memberForwarded: Int = 0
    /** Index into #String. */
    var importName: Int = 0
    /** Index into the ModuleRef table. */
    var importScope: Int = 0

    protected def populateFields(): Unit = {
      mappingFlags = readShort
      memberForwarded = readTableSetIndex(_MemberForwarded)
      importName = readStringIndex
      importScope = readTableIndex(ModuleRef.ID)
    }

    protected def getRowSize: Int = {
      return 2 + file.getTableSetIndexSize(_MemberForwarded) + file.getStringIndexSize + file.getTableIndexSize(ModuleRef.ID)
    }
  }

  object FieldRVA {
    val ID: Int = 0x1d
  }

  final class FieldRVA(file: PEFile, rows: Int) extends Table(file, FieldRVA.ID, rows) {
    /** 4-byte constant. */
    var RVA: Int = 0
    /** Index into the Field table. */
    var Field: Int = 0

    protected def populateFields(): Unit = {
      RVA = readInt
      Field = readTableIndex(Table.FieldDef.ID)
    }

    protected def getRowSize: Int = {
      return 4 + file.getTableIndexSize(FieldDef.ID)
    }
  }

  object AssemblyDef {
    val ID: Int = 0x20
  }

  final class AssemblyDef(file: PEFile, rows: Int) extends Table(file, AssemblyDef.ID, rows) {
    /** 4-byte constatnt of type AssemblyHashAlgorithm, clause 22.1.1 */
    var HashAlgId: Int = 0
    /** 2-byte constant */
    var MajorVersion: Int = 0
    /** 2-byte constant */
    var MinorVersion: Int = 0
    /** 2-byte constant */
    var BuildNumber: Int = 0
    /** 2-byte constant */
    var RevisionNumber: Int = 0
    /** 4-byte constant */
    var Flags: Int = 0
    /** index into #Blob */
    var PublicKey: Int = 0
    /** index into #String */
    var Name: Int = 0
    /** index into #String */
    var Culture: Int = 0

    protected def populateFields(): Unit = {
      HashAlgId = readInt
      MajorVersion = readShort
      MinorVersion = readShort
      BuildNumber = readShort
      RevisionNumber = readShort
      Flags = readInt
      PublicKey = readBlobIndex
      Name = readStringIndex
      Culture = readStringIndex
    }

    protected def getRowSize: Int = {
      return 16 + file.getBlobIndexSize + 2 * file.getStringIndexSize
    }
  }

  object AssemblyProcessor {
    val ID: Int = 0x21
  }

  final class AssemblyProcessor(file: PEFile, rows: Int) extends Table(file, AssemblyProcessor.ID, rows) {
    /** 4-byte constant. */
    var Processor: Int = 0

    protected def populateFields(): Unit = {
      Processor = readInt
    }

    protected def getRowSize: Int = {
      return 4
    }
  }

  object AssemblyOS {
    val ID: Int = 0x22
  }

  final class AssemblyOS(file: PEFile, rows: Int) extends Table(file, AssemblyOS.ID, rows) {
    /** 4-byte constant. */
    var OSPlatformID: Int = 0
    /** 4-byte constant. */
    var OSMajorVersion: Int = 0
    /** 4-byte constant. */
    var OSMinorVersion: Int = 0

    protected def populateFields(): Unit = {
      OSPlatformID = readInt
      OSMajorVersion = readInt
      OSMinorVersion = readInt
    }

    protected def getRowSize: Int = {
      return 12
    }
  }

  object AssemblyRef {
    val ID: Int = 0x23
  }

  final class AssemblyRef(file: PEFile, rows: Int) extends Table(file, AssemblyRef.ID, rows) {
    /** 2-byte constant. */
    var MajorVersion: Int = 0
    /** 2-byte constant. */
    var MinorVersion: Int = 0
    /** 2-byte constant. */
    var BuildNumber: Int = 0
    /** 2-byte constant. */
    var RevisionNumber: Int = 0
    /** 4-byte bitmask of type AssemblyFlags (22.1.2). */
    var Flags: Int = 0
    /** index into #Blob. */
    var PublicKeyOrToken: Int = 0
    /** index into #String. */
    var Name: Int = 0
    /** index into #String. */
    var Culture: Int = 0
    /** index into #Blob. */
    var HashValue: Int = 0

    protected def populateFields(): Unit = {
      MajorVersion = readShort
      MinorVersion = readShort
      BuildNumber = readShort
      RevisionNumber = readShort
      Flags = readInt
      PublicKeyOrToken = readBlobIndex
      Name = readStringIndex
      Culture = readStringIndex
      HashValue = readBlobIndex
    }

    protected def getRowSize: Int = {
      return 12 + 2 * file.getBlobIndexSize + 2 * file.getStringIndexSize
    }

    def getName: String = {
      return file.getString(Name)
    }
  }

  object AssemblyRefProcessor {
    val ID: Int = 0x24
  }

  final class AssemblyRefProcessor(file: PEFile, rows: Int) extends Table(file, AssemblyRefProcessor.ID, rows) {
    /** 4-byte constant. */
    var Processor: Int = 0
    /** Index into the AssemblyRef table. */
    var AssemblyRef: Int = 0

    protected def populateFields(): Unit = {
      Processor = readInt
      AssemblyRef = readTableIndex(Table.AssemblyRef.ID)
    }

    protected def getRowSize: Int = {
      return 4 + file.getTableIndexSize(Table.AssemblyRef.ID)
    }
  }

  object AssemblyRefOS {
    val ID: Int = 0x25
  }

  final class AssemblyRefOS(file: PEFile, rows: Int) extends Table(file, AssemblyRefOS.ID, rows) {
    /** 4-byte constant. */
    var OSPlatformId: Int = 0
    /** 4-byte constant. */
    var OSMajorVersion: Int = 0
    /** 4-byte constant. */
    var OSMinorVersion: Int = 0
    /** Index into the AssemblyRef table. */
    var AssemblyRef: Int = 0

    protected def populateFields(): Unit = {
      OSPlatformId = readInt
      OSMajorVersion = readInt
      OSMinorVersion = readInt
      AssemblyRef = readTableIndex(Table.AssemblyRef.ID)
    }

    protected def getRowSize: Int = {
      return 12 + file.getTableIndexSize(Table.AssemblyRef.ID)
    }
  }

  object FileDef {
    val ID: Int = 0x26
  }

  final class FileDef(file: PEFile, rows: Int) extends Table(file, FileDef.ID, rows) {
    /** 4-byte bitmask of type FileAttributes (22.1.6). */
    var Flags: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into #Blob. */
    var HashValue: Int = 0

    protected def populateFields(): Unit = {
      Flags = readInt
      Name = readStringIndex
      HashValue = readBlobIndex
    }

    protected def getRowSize: Int = {
      return 4 + file.getStringIndexSize + file.getBlobIndexSize
    }

    def getName: String = {
      return file.getString(Name)
    }
  }

  object ExportedType {
    val ID: Int = 0x27
  }

  final class ExportedType(override val file: PEFile, override val rows: Int) extends Table(file, ExportedType.ID, rows) {
    /** 4-byte bitmask of type TypeAttribute (22.1.6). */
    var Flags: Int = 0
    /** 4-byte index into a TypeDef table of another module in this assembly.
      */
    var TypeDefId: Int = 0
    /** Index into #String. */
    var TypeName: Int = 0
    /** Index into #Stream. */
    var TypeNamespace: Int = 0
    /** Index into one of two tables as follows: - 'File' table, where that entry
      * says which module in the current assembly holds the TypeDef -
      * 'ExportedType' table, where that entry is the enclosing Type of the
      * current nested Type
      */
    var Implementation: Int = 0

    protected def populateFields(): Unit = {
      Flags = readInt
      TypeDefId = readInt
      TypeName = readStringIndex
      TypeNamespace = readStringIndex
      Implementation = readTableSetIndex(_Implementation)
    }

    protected def getRowSize: Int = {
      return 8 + 2 * file.getStringIndexSize + file.getTableSetIndexSize(_Implementation)
    }

    def getFullName: String = {
      val namespace: String = file.getString(TypeNamespace)
      return if (namespace.length == 0) file.getString(TypeName) else namespace + "." + file.getString(TypeName)
    }
  }

  object ManifestResource {
    val ID: Int = 0x28
  }

  final class ManifestResource(override val file: PEFile, override val rows: Int) extends Table(file, ManifestResource.ID, rows) {
    /** 4-byte constant. */
    var Offset: Int = 0
    /** 4-byte bitmask of type ManifestResourceAttributes (22.1.8). */
    var Flags: Int = 0
    /** Index into #String. */
    var Name: Int = 0
    /** Index into the Implementation table set. */
    var Implementation: Int = 0

    protected def populateFields(): Unit = {
      Offset = readInt
      Flags = readInt
      Name = readStringIndex
      Implementation = readTableSetIndex(_Implementation)
    }

    protected def getRowSize: Int = {
      return 8 + file.getStringIndexSize + file.getTableSetIndexSize(_Implementation)
    }
  }

  object NestedClass {
    val ID: Int = 0x29
  }

  final class NestedClass(override val file: PEFile, override val rows: Int) extends Table(file, NestedClass.ID, rows) {
    /** Index into the TypeDef table. */
    var nestedClass: Int = 0
    /** Index into the TypeDef table. */
    var enclosingClass: Int = 0

    protected def populateFields(): Unit = {
      nestedClass = readTableIndex(TypeDef.ID)
      enclosingClass = readTableIndex(TypeDef.ID)
    }

    protected def getRowSize: Int = {
      return 2 * file.getTableIndexSize(TypeDef.ID)
    }
  }

  object GenericParam {
    val ID: Int = 0x2a
  }

  final class GenericParam(override val file: PEFile, override val rows: Int) extends Table(file, GenericParam.ID, rows) {
    newMapping = true

    var number: Int = 0
    var flags: Int = 0
    var owner: Int = 0
    var name: Int = 0
    private final val GenericParamIdxesForMethodDefIdx: Map[Integer, Set[Integer]] = new HashMap[Integer, Set[Integer]]
    private final val GenericParamIdxesForTypeDefIdx: Map[Integer, Set[Integer]] = new HashMap[Integer, Set[Integer]]

    private def addToMap(key: Int, value: Int, IdxesForIdx: Map[Integer, Set[Integer]]): Unit = {
      var bucket: Set[Integer] = IdxesForIdx.get(Integer.valueOf(key))
      if (bucket == null) {
        bucket = new HashSet[Integer]
        IdxesForIdx.put(Integer.valueOf(key), bucket)
      }
      bucket.add(Integer.valueOf(value))
    }

    /** Indexes of rows in the GenericParam table representing type parameters
      * defined by the type given by its row index TypeDefIdx (in the TypeDef
      * table). No need to position the current record before invoking this
      * method.
      */
    def getTVarIdxes(TypeDefIdx: Int): Array[Int] = {
      if (!mapsPopulated) {
        initMaps
      }
      var bucket: Set[Integer] = GenericParamIdxesForTypeDefIdx.get(Integer.valueOf(TypeDefIdx))
      if (bucket == null) {
        bucket = Collections.emptySet[Integer]
      }
      val res: Array[Int] = new Array[Int](bucket.size)
      val it: Iterator[Integer] = bucket.iterator

      {
        var i: Int = 0
        while (i < bucket.size) {
          {
            res(i) = it.next.intValue
          }
          ({
            i += 1;
            i - 1
          })
        }
      }
      return res
    }

    /** Indexes of rows in the GenericParam table representing type parameters
      * defined by the method given by its row index MethodDefIdx (in the
      * MethodDef table) No need to position the current record before invoking
      * this method.
      */
    def getMVarIdxes(MethodDefIdx: Int): Array[Int] = {
      if (!mapsPopulated) {
        initMaps
      }
      var bucket: Set[Integer] = GenericParamIdxesForMethodDefIdx.get(Integer.valueOf(MethodDefIdx))
      if (bucket == null) {
        bucket = Collections.emptySet[Integer]
      }
      val res: Array[Int] = new Array[Int](bucket.size)
      val it: Iterator[Integer] = bucket.iterator

      {
        var i: Int = 0
        while (i < bucket.size) {
          {
            res(i) = it.next.intValue
          }
          ({
            i += 1;
            i - 1
          })
        }
      }
      return res
    }

    private var mapsPopulated: Boolean = false

    private def initMaps(): Unit = {
      mapsPopulated = true

      {
        var currentParamRow: Int = 1
        while (currentParamRow <= rows) {
          {
            val currentOwner: Int = file.genericParam(currentParamRow).owner
            val targetTableId: Int = Table.tableId(Table._TypeOrMethodDef, currentOwner)
            val targetRow: Int = currentOwner >> Table.NoBits(Table._TypeOrMethodDef)
            if (targetTableId == TypeDef.ID) {
              addToMap(targetRow, currentParamRow, GenericParamIdxesForTypeDefIdx)
            } else if (targetTableId == MethodDef.ID) {
              addToMap(targetRow, currentParamRow, GenericParamIdxesForMethodDefIdx)
            } else {
              throw new RuntimeException
            }
          }
          ({
            currentParamRow += 1;
            currentParamRow - 1
          })
        }
      }
    }

    protected def populateFields(): Unit = {
      number = readShort
      flags = readShort
      owner = readTableSetIndex(_TypeOrMethodDef)
      name = readStringIndex
    }

    /** This method assumes populateFields() has been just called to set Flags
      * for the current record
      */
    def isInvariant: Boolean = {
      return (flags & 0x0003) == 0
    }

    /** This method assumes populateFields() has been just called to set Flags
      * for the current record
      */
    def isCovariant: Boolean = {
      return (flags & 0x0003) == 1
    }

    /** This method assumes populateFields() has been just called to set Flags
      * for the current record
      */
    def isContravariant: Boolean = {
      return (flags & 0x0003) == 2
    }

    /** This method assumes populateFields() has been just called to set Flags
      * for the current record
      */
    def isReferenceType: Boolean = {
      return (flags & 0x001C) == 4
    }

    /** This method assumes populateFields() has been just called to set Flags
      * for the current record
      */
    def isValueType: Boolean = {
      return (flags & 0x001C) == 8
    }

    /** This method assumes populateFields() has been just called to set Flags
      * for the current record
      */
    def hasDefaultConstructor: Boolean = {
      return (flags & 0x001C) == 0x0010
    }

    protected def getRowSize: Int = {
      return 2 + 2 + file.getTableSetIndexSize(_TypeOrMethodDef) + file.getStringIndexSize
    }

    def getName: String = {
      return file.getString(name)
    }
  }

  object GenericParamConstraint {
    val ID: Int = 0x2c
  }

  final class GenericParamConstraint(file: PEFile, rows: Int) extends Table(file, GenericParamConstraint.ID, rows) {
    var owner: Int = 0
    var constraint: Int = 0
    this.newMapping = true

    protected def populateFields(): Unit = {
      owner = readTableIndex(GenericParam.ID)
      constraint = readTableSetIndex(_TypeDefOrRef)
    }

    protected def getRowSize: Int = {
      return file.getTableIndexSize(GenericParam.ID) + file.getTableSetIndexSize(_TypeDefOrRef)
    }

    private var mapPopulated: Boolean = false

    /** Indexes of rows (in the TypeDef, TypeRef, or TypeSpec tables) denoting
      * the base class (if any) and interfaces (if any) that the generic
      * parameter (of TVar or MVar kind) should support, where that generic
      * parameter is represented by its index into the GenericParam table.
      */
    def getTypeDefOrRefIdxes(genParamIdx: Int): Array[Int] = {
      if (!mapPopulated) {
        initMap
      }
      var bucket: Set[Integer] = TypeDefOrRefIdxesForGenParamIdx.get(Integer.valueOf(genParamIdx))
      if (bucket == null) {
        bucket = Collections.emptySet[Integer]
      }
      val res: Array[Int] = new Array[Int](bucket.size)
      val it: Iterator[Integer] = bucket.iterator

      {
        var i: Int = 0
        while (i < bucket.size) {
          res(i) = it.next.intValue
          i += 1
        }
      }
      return res
    }

    private def initMap(): Unit = {
      mapPopulated = true

      {
        var currentConstraintRow: Int = 1
        while (currentConstraintRow <= rows) {
          {
            val targetGenericParam: Int = file.genericParamConstraint(currentConstraintRow).owner
            val value: Int = file.genericParamConstraint.constraint
            addToMap(targetGenericParam, value)
          }
          ({
            currentConstraintRow += 1;
            currentConstraintRow - 1
          })
        }
      }
    }

    private final val TypeDefOrRefIdxesForGenParamIdx: java.util.Map[Integer, Set[Integer]] = new util.HashMap[Integer, Set[Integer]]

    private def addToMap(key: Int, value: Int): Unit = {
      var bucket: java.util.Set[Integer] = TypeDefOrRefIdxesForGenParamIdx.get(Integer.valueOf(key))
      if (bucket == null) {
        bucket = new util.HashSet[Integer]
        TypeDefOrRefIdxesForGenParamIdx.put(Integer.valueOf(key), bucket)
      }
      bucket.add(Integer.valueOf(value))
    }
  }

  object MethodSpec {
    val ID: Int = 0x2b
  }

  final class MethodSpec(override val file: PEFile, override val rows: Int) extends Table(file, MethodSpec.ID, rows) {
    var method: Int = 0
    var instantiation: Int = 0
    this.newMapping = true

    protected def populateFields(): Unit = {
      method = readTableSetIndex(_MethodDefOrRef)
      instantiation = readBlobIndex
    }

    protected def getRowSize: Int = {
      return file.getTableSetIndexSize(_MethodDefOrRef) + file.getBlobIndexSize
    }
  }

}

/**
  * @param file The file to which the table belongs.
  * @param id   Table ID as specified in Partition II.
  * @param rows Number of rows in the table.
  */
abstract class Table(val file: PEFile, val id: Int, val rows: Int) {
  /** Memory mapped buffer wrapping the table. */
  protected var buffer: ByteBuffer = null
  /** specified wheter a new memory-mapped byte buffer should be created for this
    * table.
    */
  protected var newMapping: Boolean = false
  /** Tells wheter the table is indexed by 2-byte (short) integer or by 4-byte
    * integer.
    */
  final val isShort: Boolean = rows < (1 << 16)
  private var _rowSize: Int = -1
  private var start: Long = -1
  private var _currentRow: Int = 0

  /** Additional table initialization.
    *
    * @return the starting position of the next table in the stream.
    */
  final def init(start: Long): Long = {
    if (rows < 1) return start
    if (this.start == -1) this.start = start
    else throw new RuntimeException("Cannot re-initialize table \'" + getTableName + "\'")
    _rowSize = getRowSize
    val size: Int = rows * _rowSize
    buffer = if (this.newMapping) file.mapBuffer(start, size) else file.getBuffer(start, size)
    return start + size
  }

  final def getTableName: String =
    if (0 <= id && id < Table.MAX_NUMBER) Table.tableName(id) else "<NoTable>"

  /** @return the size of the row in bytes
    */
  final def rowSize: Int = _rowSize

  /** if the underlying buffer is memory-mapped, load its contents into memory
    */
  def load(): Unit =
    if (buffer.isInstanceOf[MappedByteBuffer]) (buffer.asInstanceOf[MappedByteBuffer]).load

  /** */
  final def readByte: Int = (buffer.get + 0x100) & 0xff

  /** */
  final def readShort: Int = (buffer.getShort + 0x10000) & 0xffff

  /** */
  final def readInt: Int = buffer.getInt

  /** */
  final def readStringIndex: Int = if (file.StringIsShort) readShort else readInt

  /** */
  final def readBlobIndex: Int = if (file.BlobIsShort) readShort else readInt

  /** */
  final def readGUIDIndex: Int = if (file.GUIDIsShort) readShort else readInt

  /** */
  final def readTableIndex(tableId: Int): Int =
    if (file.getTable(tableId).isShort) readShort else readInt

  /** */
  final def readTableSetIndex(tableSetId: Int): Int =
    if (file.indexSize(tableSetId) == 2) readShort else readInt

  /** Read the specified row and populate the fields of the instance. */
  final def readRow(row: Int): Unit = {
    seekRow(row)
    val lastSeek: Int = buffer.position
    populateFields()
    val rowSizeRead: Int = buffer.position - lastSeek
    if (rowSizeRead != _rowSize) throw new RuntimeException("Table ID=0x" + PEFile.byte2hex(id) + ": read row size = " + rowSizeRead + "; expected row size = " + _rowSize)
    _currentRow = row
  }

  /** Seeks in the file the position of the specified row. */
  protected final def seekRow(row: Int): Unit = {
    assert(row > 0 && row <= rows, "Index " + row + " is not within the table with #rows = " + rows)
    buffer.position((row - 1) * _rowSize)
  }

  final def currentRow: Int = _currentRow

  final def nextRow(): Unit = readRow(_currentRow + 1)

  /** Assigns values to the fields of the class. */
  protected def populateFields(): Unit

  /** Returns the size of a row in bytes. */
  protected def getRowSize: Int
}