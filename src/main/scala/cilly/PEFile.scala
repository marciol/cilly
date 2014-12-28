/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.io.{ File, IOException, RandomAccessFile }
import java.nio.channels.FileChannel
import java.nio.{ ByteBuffer, MappedByteBuffer }
import java.util.Date

import cilly.util._

/** A class that represents a .NET PE/COFF image.
  *
  * @author Nikolay Mihaylov
  * @version 1.0
  * @see <a href="http://www.ecma-international.org/publications/standards/Ecma-335.htm">Standard ECMA-335:  Common Language Infrastructure (CLI), 4th edition (June 2006)</a>
  */
object PEFile {
  val INT_SIZE: Int = 4

  private def fileFormatCheck(cond: Boolean, s: String): Unit = {
    if (cond) throw new RuntimeException(s)
  }

  def long2hex(a: Long): String = {
    val str: StringBuffer = new StringBuffer("0000000000000000")
    str.append(java.lang.Long.toHexString(a))
    val l: Int = str.length
    str.substring(l - 16, l)
  }

  def int2hex(a: Int): String = {
    val str: StringBuffer = new StringBuffer("00000000")
    str.append(Integer.toHexString(a))
    val l: Int = str.length
    str.substring(l - 8, l)
  }

  def short2hex(a: Int): String = {
    val str: StringBuffer = new StringBuffer("0000")
    str.append(Integer.toHexString(a))
    val l: Int = str.length
    str.substring(l - 4, l)
  }

  def byte2hex(a: Int): String = {
    val str: StringBuffer = new StringBuffer("00")
    str.append(Integer.toHexString(a))
    val l: Int = str.length
    str.substring(l - 2, l)
  }

  def bytes2hex(buf: Array[Byte]): String = {
    val str: StringBuffer = new StringBuffer

    {
      var i: Int = 0
      while (i < buf.length) {
        {
          str.append(byte2hex(buf(i)))
          if (i < buf.length - 1) str.append(" ")
        }
        {
          i += 1
          i - 1
        }
      }
    }
    str.toString
  }
}

class PEFile(filename: String) {
  protected final var PE_SIGNATURE_OFFSET: Int = 0
  protected final var COFF_HEADER_OFFSET: Int = 0
  protected final var PE_HEADER_OFFSET: Int = 0
  protected final var numOfSections: Int = 0
  protected final var CLI_RVA: Int = 0
  protected final var CLI_Length: Int = 0
  final var rvaMetadata: Int = 0
  final var posMetadata: Int = 0
  protected final var numOfStreams: Int = 0
  protected final var optHeaderSize: Int = 0
  private[cilly] final val underlyingFile: File = new File(filename)
  protected final val file: RandomAccessFile = new RandomAccessFile(underlyingFile, "r")
  protected final var buf: MappedByteBuffer = {
    val fc: FileChannel = file.getChannel
    val bb: MappedByteBuffer =
      try {
        fc.map(FileChannel.MapMode.READ_ONLY, 0L, fc.size)
      } catch {
        case e: IOException =>
          throw new RuntimeException(e)
      }
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    bb
  }
  protected final var sections: Array[PESection] = null
  var Meta: PEStream = null
  var Strings: PEStream = null
  var US: PEStream = null
  var Blob: PEStream = null
  var GUID: PEStream = null
  private final val tables: Array[Table] = new Array[Table](Table.MAX_NUMBER)
  final var isDLL: Boolean = false
  protected final var heapSizes: Int = 0
  final var StringIsShort: Boolean = false
  final var BlobIsShort: Boolean = false
  final var GUIDIsShort: Boolean = false
  protected var pemodule: PEModule = null
  final val indexSize: Array[Int] = new Array[Int](Table.TABLE_SET_LENGTH)

  {
    seek(0)
    PEFile.fileFormatCheck(readByte != 0x4d, "Invalid PE file format: " + filename)
    PEFile.fileFormatCheck(readByte != 0x5a, "Invalid PE file format: " + filename)
    seek(0x3c)
    PE_SIGNATURE_OFFSET = readInt
    seek(PE_SIGNATURE_OFFSET)
    PEFile.fileFormatCheck(readByte != 0x50, "Invalid PE file format: " + filename)
    PEFile.fileFormatCheck(readByte != 0x45, "Invalid PE file format: " + filename)
    PEFile.fileFormatCheck(readByte != 0x00, "Invalid PE file format: " + filename)
    PEFile.fileFormatCheck(readByte != 0x00, "Invalid PE file format: " + filename)
    COFF_HEADER_OFFSET = PE_SIGNATURE_OFFSET + 4
    PE_HEADER_OFFSET = COFF_HEADER_OFFSET + 20
    seek(COFF_HEADER_OFFSET)
    skip(2)
    numOfSections = readShort
    val timeStamp: Date = new Date(readInt * 1000L)
    skip(2 * PEFile.INT_SIZE)
    optHeaderSize = readShort
    val characteristics: Int = readShort
    isDLL = (characteristics & 0x2000) != 0
    seek(PE_HEADER_OFFSET + 208)
    CLI_RVA = readInt
    CLI_Length = readInt
    sections = new Array[PESection](numOfSections)
    seek(PE_HEADER_OFFSET + optHeaderSize)

    {
      var i: Int = 0
      while (i < numOfSections) {
        {
          seek(PE_HEADER_OFFSET + optHeaderSize + i * 40)
          sections(i) = new PESection(this)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    seek(fromRVA(CLI_RVA))
    skip(8)
    rvaMetadata = readInt
    posMetadata = fromRVA(rvaMetadata)
    seek(posMetadata)
    val magic: Int = readInt
    PEFile.fileFormatCheck(magic != 0x424a5342, "Invalid metadata signature!")
    skip(8)
    val strlength: Int = readInt
    skip(strlength)
    align(PEFile.INT_SIZE, posMetadata)
    skip(2)
    numOfStreams = readShort

    {
      var i: Int = 0
      while (i < numOfStreams) {
        {
          val strm: PEStream = new PEStream(this)
          if ((strm.name == "#~") || (strm.name == "#-")) Meta = strm
          if (strm.name == "#Strings") Strings = strm
          if (strm.name == "#US") US = strm
          if (strm.name == "#Blob") Blob = strm
          if (strm.name == "#GUID") GUID = strm
        }
        {
          i += 1
          i - 1
        }
      }
    }
    seek(Meta.offset)
    skip(6)
    heapSizes = readByte
    StringIsShort = (heapSizes & 0x01) == 0
    GUIDIsShort = (heapSizes & 0x02) == 0
    BlobIsShort = (heapSizes & 0x04) == 0
    skip(1)
    val tablesMask: Long = readLong
    val nonStandardTables: Long = tablesMask & ~Table.VALID_TABLES_MASK
    skip(8)

    {
      var i: Int = 0
      while (i < tables.length) {
        {
          tables(i) = Table.newTable(this, i, if (((tablesMask >> i) & 0x01) != 0) readInt else 0)
        }
        {
          i += 1
          i - 1
        }
      }
    }
    initIndexSize()
    initTableRefs()
    var start: Long = pos

    {
      var i: Int = 0
      while (i < tables.length) {
        start = tables(i).init(start)
        i += 1
      }
    }
  }
  /// ctor end

  private def initIndexSize(): Unit = {
    var i: Int = 0
    while (i < Table.TABLE_SET_LENGTH) {
      indexSize(i) = 2
      val tableSet: Array[Int] = Table.TableSet(i)
      val treshold: Int = 65536 >> Table.NoBits(i)

      {
        var j: Int = 0
        var continue = true
        while (j < tableSet.length && continue) {
          if (tableSet(j) >= 0) {
            val t: Table = tables(tableSet(j))
            if (t.rows >= treshold) {
              indexSize(i) = 4
              continue = false
            }
          }
          j += 1
        }
      }
      i += 1
    }
  }

  private[cilly] def initModule(module: PEModule): Unit = {
    if (pemodule != null) throw new RuntimeException("File " + this + " has already been assigned module " + pemodule + "; new module is " + module)
    this.pemodule = module
  }

  var moduleDef: Table.ModuleDef = null

  def moduleDef(i: Int): Table.ModuleDef = {
    moduleDef.readRow(i)
    moduleDef
  }

  var typeRef: Table.TypeRef = null
  var typeDef: Table.TypeDef = null

  def typeDef(i: Int): Table.TypeDef = {
    typeDef.readRow(i)
    typeDef
  }

  var fieldTrans: Table.FieldTrans = null

  def fieldTrans(i: Int): Table.FieldTrans = {
    fieldTrans.readRow(i)
    fieldTrans
  }

  var fieldDef: Table.FieldDef = null

  def fieldDef(i: Int): Table.FieldDef = {
    fieldDef.readRow(i)
    fieldDef
  }

  var methodTrans: Table.MethodTrans = null

  def methodTrans(i: Int): Table.MethodTrans = {
    methodTrans.readRow(i)
    methodTrans
  }

  var methodDef: Table.MethodDef = null

  def methodDef(i: Int): Table.MethodDef = {
    methodDef.readRow(i)
    methodDef
  }

  var paramDef: Table.ParamDef = null

  def paramDef(i: Int): Table.ParamDef = {
    paramDef.readRow(i)
    paramDef
  }

  var genericParam: Table.GenericParam = null

  def genericParam(i: Int): Table.GenericParam = {
    genericParam.readRow(i)
    genericParam
  }

  var methodSpec: Table.MethodSpec = null

  def methodSpec(i: Int): Table.MethodSpec = {
    methodSpec.readRow(i)
    methodSpec
  }

  var genericParamConstraint: Table.GenericParamConstraint = null

  def genericParamConstraint(i: Int): Table.GenericParamConstraint = {
    genericParamConstraint.readRow(i)
    genericParamConstraint
  }

  var InterfaceImpl: Table.InterfaceImpl = null
  var MemberRef: Table.MemberRef = null
  var Constant: Table.Constant = null
  var CustomAttribute: Table.CustomAttribute = null
  var FieldMarshal: Table.FieldMarshal = null
  var DeclSecurity: Table.DeclSecurity = null
  var ClassLayout: Table.ClassLayout = null
  var FieldLayout: Table.FieldLayout = null
  var StandAloneSig: Table.StandAloneSig = null
  var EventMap: Table.EventMap = null
  var EventDef: Table.EventDef = null
  var PropertyMap: Table.PropertyMap = null
  var PropertyDef: Table.PropertyDef = null
  var MethodSemantics: Table.MethodSemantics = null
  var MethodImpl: Table.MethodImpl = null
  var ModuleRef: Table.ModuleRef = null
  var TypeSpec: Table.TypeSpec = null
  var ImplMap: Table.ImplMap = null
  var FieldRVA: Table.FieldRVA = null
  var AssemblyDef: Table.AssemblyDef = null
  var AssemblyRef: Table.AssemblyRef = null
  var FileDef: Table.FileDef = null
  var ExportedType: Table.ExportedType = null
  var ManifestResource: Table.ManifestResource = null
  var NestedClass: Table.NestedClass = null

  private def initTableRefs(): Unit = {
    moduleDef = getTable(Table.ModuleDef.ID).asInstanceOf[Table.ModuleDef]
    typeRef = getTable(Table.TypeRef.ID).asInstanceOf[Table.TypeRef]
    typeDef = getTable(Table.TypeDef.ID).asInstanceOf[Table.TypeDef]
    fieldTrans = getTable(Table.FieldTrans.ID).asInstanceOf[Table.FieldTrans]
    fieldDef = getTable(Table.FieldDef.ID).asInstanceOf[Table.FieldDef]
    methodTrans = getTable(Table.MethodTrans.ID).asInstanceOf[Table.MethodTrans]
    methodDef = getTable(Table.MethodDef.ID).asInstanceOf[Table.MethodDef]
    paramDef = getTable(Table.ParamDef.ID).asInstanceOf[Table.ParamDef]
    InterfaceImpl = getTable(Table.InterfaceImpl.ID).asInstanceOf[Table.InterfaceImpl]
    MemberRef = getTable(Table.MemberRef.ID).asInstanceOf[Table.MemberRef]
    Constant = getTable(Table.Constant.ID).asInstanceOf[Table.Constant]
    CustomAttribute = getTable(Table.CustomAttribute.ID).asInstanceOf[Table.CustomAttribute]
    FieldMarshal = getTable(Table.FieldMarshal.ID).asInstanceOf[Table.FieldMarshal]
    DeclSecurity = getTable(Table.DeclSecurity.ID).asInstanceOf[Table.DeclSecurity]
    ClassLayout = getTable(Table.ClassLayout.ID).asInstanceOf[Table.ClassLayout]
    FieldLayout = getTable(Table.FieldLayout.ID).asInstanceOf[Table.FieldLayout]
    StandAloneSig = getTable(Table.StandAloneSig.ID).asInstanceOf[Table.StandAloneSig]
    EventMap = getTable(Table.EventMap.ID).asInstanceOf[Table.EventMap]
    EventDef = getTable(Table.EventDef.ID).asInstanceOf[Table.EventDef]
    PropertyMap = getTable(Table.PropertyMap.ID).asInstanceOf[Table.PropertyMap]
    PropertyDef = getTable(Table.PropertyDef.ID).asInstanceOf[Table.PropertyDef]
    MethodSemantics = getTable(Table.MethodSemantics.ID).asInstanceOf[Table.MethodSemantics]
    MethodImpl = getTable(Table.MethodImpl.ID).asInstanceOf[Table.MethodImpl]
    ModuleRef = getTable(Table.ModuleRef.ID).asInstanceOf[Table.ModuleRef]
    TypeSpec = getTable(Table.TypeSpec.ID).asInstanceOf[Table.TypeSpec]
    ImplMap = getTable(Table.ImplMap.ID).asInstanceOf[Table.ImplMap]
    FieldRVA = getTable(Table.FieldRVA.ID).asInstanceOf[Table.FieldRVA]
    AssemblyDef = getTable(Table.AssemblyDef.ID).asInstanceOf[Table.AssemblyDef]
    AssemblyRef = getTable(Table.AssemblyRef.ID).asInstanceOf[Table.AssemblyRef]
    FileDef = getTable(Table.FileDef.ID).asInstanceOf[Table.FileDef]
    ExportedType = getTable(Table.ExportedType.ID).asInstanceOf[Table.ExportedType]
    NestedClass = getTable(Table.NestedClass.ID).asInstanceOf[Table.NestedClass]
    ManifestResource = getTable(Table.ManifestResource.ID).asInstanceOf[Table.ManifestResource]
    genericParam = getTable(Table.GenericParam.ID).asInstanceOf[Table.GenericParam]
    methodSpec = getTable(Table.MethodSpec.ID).asInstanceOf[Table.MethodSpec]
    genericParamConstraint = getTable(Table.GenericParamConstraint.ID).asInstanceOf[Table.GenericParamConstraint]
  }

  /** @return the absolute path of the file
    */
  def absolutePath: String = underlyingFile.getAbsolutePath

  /** @return the name of this file
    */
  def name: String = underlyingFile.getName

  /** @return
    */
  def parent: String = underlyingFile.getParent

  /** @return the file representing the directory the file belongs to
    */
  def parentFile: File = underlyingFile.getParentFile

  override def toString: String = absolutePath

  /** Returns the current position in the file. */
  def pos: Int = buf.position

  /** Go to the specified position in the file. */
  def seek(pos: Int): Unit = buf.position(pos)

  /** Align the current position in the file. */
  def align(base: Int): Unit = align(base, 0)

  /** Align the current position in a section starting at offset. */
  def align(base: Int, offset: Int): Unit = {
    val p: Int = pos - offset
    seek(offset + (if ((p % base) == 0) p else (p / base + 1) * base))
  }

  /** Computes the position in the file that corresponds to the given RVA. */
  def fromRVA(rva: Int): Int = {
    var i: Int = 0
    while (i < numOfSections) {
      if (sections(i).virtAddr <= rva && rva <= (sections(i).virtAddr + sections(i).virtSize)) return rva - sections(i).virtAddr + sections(i).realAddr
      i += 1
    }
    throw new RuntimeException("RVA 0x" + Integer.toHexString(rva) + " is not within this file's sections!")
  }

  /** Go to the specified RVA (Relative Virtual Address). */
  def gotoRVA(rva: Int): Unit = seek(fromRVA(rva))

  /** Move the forward in the file by the specified number of bytes. */
  def skip(n: Int): Unit = buf.position(buf.position + n)

  /** Returns a memory mapped little-endian buffer
    * for the specified region of the file.
    */
  def mapBuffer(offset: Long, size: Int): MappedByteBuffer = {
    try {
      val b: MappedByteBuffer = file.getChannel.map(FileChannel.MapMode.READ_ONLY, offset, size)
      b.order(java.nio.ByteOrder.LITTLE_ENDIAN)
      b
    } catch {
      case e: IOException =>
        throw new RuntimeException(e)
    }
  }

  /** Returns a buffer from the given offset to the end of the file. */
  def getBuffer(offset: Long, size: Int): ByteBuffer = {
    buf.mark
    buf.position(offset.toInt)
    val bb: ByteBuffer = buf.slice
    buf.reset
    bb.limit(size)
    bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)
    bb
  }

  /** Read bs.length number of bytes
    */
  def read(bs: Array[Byte]): Unit = buf.get(bs)

  /** Read 1-byte integer from the current position in the file.
    */
  def readByte: Int = buf.get

  /** Read 2-byte integer from the current position in the file.
    */
  def readShort: Int = buf.getShort

  /** Read 4-byte integer from the current position in the file.
    */
  def readInt: Int = buf.getInt

  /** Read 8-byte integer from the current position in the file.
    */
  def readLong: Long = buf.getLong

  /** @return the size of string indeces for this file.
    */
  def getStringIndexSize: Int = if (StringIsShort) 2 else 4

  /** @return the size of GUID indeces for this file.
    */
  def getGUIDIndexSize: Int = if (GUIDIsShort) 2 else 4

  /** @return the size of Blob indeces for this file.
    */
  def getBlobIndexSize: Int = if (BlobIsShort) 2 else 4

  /** @return the size of the index to tableID for this file;
    * @param tableID the ID of the table
    */
  def getTableIndexSize(tableID: Int): Int = if (tables(tableID).isShort) 2 else 4

  /** @return the size of the index to a set of tables with the given @param TableSetID
    * @param tableSetID the ID of the table set
    */
  def getTableSetIndexSize(tableSetID: Int): Int = indexSize(tableSetID)

  /** Read a String index from the current position in the file.
    * @return an index into the String stream
    */
  def readStringIndex: Int = if (StringIsShort) readShort else readInt

  /** Read a GUID index from the current position in the file.
    * @return an index in to the GUID stream
    */
  def readGUIDIndex: Int = if (GUIDIsShort) readShort else readInt

  /** Read a Blob index from the current position in the file.
    * @return an index into the Blob stream
    */
  def readBlobIndex: Int = if (BlobIsShort) readShort else readInt

  /** Read an entry interpreted as index into table @param tableID. */
  def readTableIndex(tableId: Int): Int = if (tables(tableId).isShort) readShort else readInt

  /** */
  def readTableSetIndex(tableSetId: Int): Int = if (indexSize(tableSetId) == 2) readShort else readInt

  /** Read a string from the String stream
    * @return the string at the given position
    * @param pos the position of the string in the String stream
    */
  def getString(pos: Int): String = Strings.getString(pos)

  /** Read a string from the US (User Strings) stream
    * @return the string at the given position
    * @param pos the position of the string in the US stream
    */
  def getUString(pos: Int): String = US.getString(pos)

  /** Read a blob from the Blob Stream
    * @return the blob at the given position
    * @param pos the position of the blob in the Blob stream
    */
  def getBlob(pos: Int): Array[Byte] = Blob.getBlob(pos)

  /** */
  def getSignature(pos: Int): PEFile#Sig = Blob.getSignature(pos)

  /** */
  def getGUID(pos: Int): Array[Byte] = GUID.getGUID(pos)

  /** @return the table with the corresponding ID.
    */
  final def getTable(tableID: Int): Table = tables(tableID)

  /** */
  private[cilly] def trace(msg: String): Unit = println(s"[trace] $msg")

  def newSignature(buf: ByteBuffer): PEFile#Sig = new Sig(buf)

  class Sig(val buf: ByteBuffer) {

    import cilly.util.Signature._

    protected final val _pos: Int = buf.position
    protected final val length: Int = decodeInt

    override def toString: String = {
      val b: StringBuffer = new StringBuffer("(")
      val savedPos: Int = buf.position
      reset

      {
        var i: Int = 0
        while (i < length) {
          {
            b.append(PEFile.byte2hex(readByte))
            if (i < length - 1) b.append(" ")
          }
          {
            i += 1
            i - 1
          }
        }
      }
      buf.position(savedPos)
      b.append(")").toString
    }

    def reset: PEFile#Sig = {
      buf.position(pos)
      this
    }

    def pos: Int = buf.position - _pos

    /** @return the byte at the current position in the signature Blob.
      *        Stay at the same position
      */
    def getByte: Int = (buf.get(buf.position) + 0x100) & 0xff

    /** @return the byte at the current position in the signature Blob.
      *        Move to the next byte.
      */
    def readByte: Int = (buf.get + 0x100) & 0xff

    /** Skip the current byte if equal to the given value. */
    def skipByte(b: Int): Unit = if (b == getByte) buf.get

    /** Decodes an integer from the signature Blob.
      * @return the decoded integer
      */
    def decodeInt: Int = {
      var res: Int = readByte
      if ((res & 0x80) != 0) {
        res = ((res & 0x7f) << 8) | readByte
        if ((res & 0x4000) != 0) res = ((res & 0x3fff) << 16) | (readByte << 8) | readByte
      }
      res
    }

    /** @return - the type encoded at the current position in the signature
      *        according to 23.2.12
      */
    def decodeType: Type = {
      try {
        decodeType0
      } catch {
        case e: RuntimeException =>
          System.out.println("" + pos + "@" + this)
          throw e
      }
    }

    def decodeType0: Type = {
      val desc: Int = readByte
      val typ: Type = desc match {
        case ELEMENT_TYPE_BOOLEAN =>
          Type.getType("System.Boolean")
        case ELEMENT_TYPE_CHAR =>
          Type.getType("System.Char")
        case ELEMENT_TYPE_I1 =>
          Type.getType("System.SByte")
        case ELEMENT_TYPE_U1 =>
          Type.getType("System.Byte")
        case ELEMENT_TYPE_I2 =>
          Type.getType("System.Int16")
        case ELEMENT_TYPE_U2 =>
          Type.getType("System.UInt16")
        case ELEMENT_TYPE_I4 =>
          Type.getType("System.Int32")
        case ELEMENT_TYPE_U4 =>
          Type.getType("System.UInt32")
        case ELEMENT_TYPE_I8 =>
          Type.getType("System.Int64")
        case ELEMENT_TYPE_U8 =>
          Type.getType("System.UInt64")
        case ELEMENT_TYPE_R4 =>
          Type.getType("System.Single")
        case ELEMENT_TYPE_R8 =>
          Type.getType("System.Double")
        case ELEMENT_TYPE_OBJECT =>
          Type.getType("System.Object")
        case ELEMENT_TYPE_STRING =>
          Type.getType("System.String")
        case ELEMENT_TYPE_I =>
          Type.getType("System.IntPtr")
        case ELEMENT_TYPE_U =>
          Type.getType("System.UIntPtr")
        case ELEMENT_TYPE_PTR =>
          if (getByte == ELEMENT_TYPE_VOID) {
            readByte
            Type.mkPtr(Type.getType("System.Void"))
          } else Type.mkPtr(decodeType)
        case ELEMENT_TYPE_BYREF =>
          Type.mkByRef(decodeType)
        case ELEMENT_TYPE_VALUETYPE | ELEMENT_TYPE_CLASS =>
          val t = pemodule.getTypeDefOrRef(decodeInt)
          if (t == null) throw new RuntimeException
          t
        case ELEMENT_TYPE_SZARRAY =>
          skipCustomMods()
          Type.mkArray(decodeType, 1)
        case ELEMENT_TYPE_ARRAY =>
          val elem: Type = decodeType
          val rank: Int = decodeInt
          val numSizes: Int = decodeInt

          {
            var i: Int = 0
            while (i < numSizes) {
              decodeInt
              i += 1
            }
          }
          val numLoBounds: Int = decodeInt

          {
            var i: Int = 0
            while (i < numLoBounds) {
              decodeInt
              i += 1
            }
          }
          Type.mkArray(elem, rank)
        case ELEMENT_TYPE_GENERICINST =>
          val b: Int = readByte
          val instantiatedType: Type = pemodule.getTypeDefOrRef(decodeInt)
          val numberOfTypeArgs: Int = decodeInt
          val typeArgs: Array[Type] = new Array[Type](numberOfTypeArgs)

          {
            var iarg: Int = 0
            while (iarg < numberOfTypeArgs) {
              {
                typeArgs(iarg) = decodeType
              }
              {
                iarg += 1
                iarg - 1
              }
            }
          }
          new ConstructedType(instantiatedType, typeArgs)
        case ELEMENT_TYPE_VAR =>
          var typeArgAsZeroBased: Int = decodeInt
          new Type.TMVarUsage(typeArgAsZeroBased, true)
        case ELEMENT_TYPE_MVAR =>
          var typeArgAsZeroBased: Int = decodeInt
          new Type.TMVarUsage(typeArgAsZeroBased, false)
        case _ =>
          throw new RuntimeException(PEFile.byte2hex(desc) + "@" + pos + " in " + this)
      }
      if (typ == null) throw new RuntimeException
      typ
    }

    def decodeFieldType: PECustomMod = {
      skipByte(FIELD)
      val cmods: Array[CustomModifier] = getCustomMods
      val fieldType: Type = decodeType
      new PECustomMod(fieldType, cmods)
    }

    /** decodes the return type of a method signature (22.2.11). */
    def decodeRetType: Type = {
      skipCustomMods()
      getByte match {
        case ELEMENT_TYPE_VOID =>
          readByte
          Type.getType("System.Void")
        case ELEMENT_TYPE_TYPEDBYREF =>
          Type.getType("System.TypedReference")
        case ELEMENT_TYPE_BYREF =>
          decodeType
        case _ =>
          decodeType
      }
    }

    def decodeParamType: Type = {
      skipCustomMods()
      getByte match {
        case ELEMENT_TYPE_BYREF =>
          decodeType
        case ELEMENT_TYPE_TYPEDBYREF =>
          Type.getType("System.TypedReference")
        case _ =>
          decodeType
      }
    }

    def skipCustomMods(): Unit = {
      while (getByte == ELEMENT_TYPE_CMOD_OPT || getByte == ELEMENT_TYPE_CMOD_REQD) {
        val isREQD: Boolean = getByte == ELEMENT_TYPE_CMOD_REQD
        readByte
        val ignored: Type = pemodule.getTypeDefOrRef(decodeInt)
        if (isREQD) {
        }
      }
    }

    /** @see CustomModifier
      */
    def getCustomMods: Array[CustomModifier] = {
      val cmods: java.util.List[CustomModifier] = new java.util.LinkedList[CustomModifier]
      while (getByte == ELEMENT_TYPE_CMOD_OPT || getByte == ELEMENT_TYPE_CMOD_REQD) {
        val isReqd: Boolean = getByte == ELEMENT_TYPE_CMOD_REQD
        readByte
        val t: Type = pemodule.getTypeDefOrRef(decodeInt)
        cmods.add(new CustomModifier(isReqd, t))
      }
      val res: Array[CustomModifier] = cmods.toArray(new Array[CustomModifier](0))
      res
    }
  }

}