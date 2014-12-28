/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.io.UnsupportedEncodingException
import java.nio.{ByteBuffer, ByteOrder}
import java.util.{HashMap, LinkedHashMap, Map}

import cilly.Attribute.NamedArgument
import cilly.util.Signature

/**
 * Describes custom attribute instances.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object Attribute {
  private val type2id: Map[Type, Integer] = new HashMap[Type, Integer]
  private val id2type: Map[Integer, Type] = new HashMap[Integer, Type]

  map("Boolean", Signature.ELEMENT_TYPE_BOOLEAN)
    map("Char", Signature.ELEMENT_TYPE_CHAR)
    map("SByte", Signature.ELEMENT_TYPE_I1)
    map("Byte", Signature.ELEMENT_TYPE_U1)
    map("Int16", Signature.ELEMENT_TYPE_I2)
    map("UInt16", Signature.ELEMENT_TYPE_U2)
    map("Int32", Signature.ELEMENT_TYPE_I4)
    map("UInt32", Signature.ELEMENT_TYPE_U4)
    map("Int64", Signature.ELEMENT_TYPE_I8)
    map("UInt64", Signature.ELEMENT_TYPE_U8)
    map("Single", Signature.ELEMENT_TYPE_R4)
    map("Double", Signature.ELEMENT_TYPE_R8)
    map("String", Signature.ELEMENT_TYPE_STRING)
    map("Type", Signature.X_ELEMENT_TYPE_TYPE)
    map("Object", Signature.ELEMENT_TYPE_OBJECT)

  private def map(typ: String, id: Int): Unit = {
    val t: Type = Type.getType("System." + typ)
    assert(typ != null, typ + " -> " + id)
    val i: Integer = new Integer(id)
    type2id.put(t, i)
    id2type.put(i, t)
  }

  private def getTypeId(typ: Type): Int = {
    val id: Integer = type2id.get(typ)
    assert(id != null, typ)
    return id.intValue
  }

  private def formatValue(str: StringBuffer, o: AnyRef): Unit = {
    val c: Class[_] = if ((o == null)) null else o.getClass
    if (c == null) {
      str.append("<null>")
    }
    else if (c eq classOf[String]) {
      str.append('"')
      str.append(o)
      str.append('"')
    }
    else if (c eq classOf[Character]) {
      str.append('\'')
      str.append(o)
      str.append('\'')
    }
    else if (c eq classOf[Array[Boolean]]) {
      str.append("new boolean[] {")
      val arr: Array[Boolean] = o.asInstanceOf[Array[Boolean]]

      {
        var i: Int = 0
        while (i < arr.length) {
          {
            if (i > 0) str.append(", ")
            str.append(arr(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      str.append('}')
    }
    else if (c eq classOf[Array[Char]]) {
      str.append("new short[] {")
      val arr: Array[Short] = o.asInstanceOf[Array[Short]]

      {
        var i: Int = 0
        while (i < arr.length) {
          {
            if (i > 0) str.append(", ")
            str.append(arr(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      str.append('}')
    }
    else if (c eq classOf[Array[Byte]]) {
      str.append("new byte[] {")
      val arr: Array[Byte] = o.asInstanceOf[Array[Byte]]

      {
        var i: Int = 0
        while (i < arr.length) {
          {
            if (i > 0) str.append(", ")
            str.append(arr(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      str.append('}')
    }
    else if (c eq classOf[Array[Short]]) {
      str.append("new short[] {")
      val arr: Array[Short] = o.asInstanceOf[Array[Short]]

      {
        var i: Int = 0
        while (i < arr.length) {
          {
            if (i > 0) str.append(", ")
            str.append(arr(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      str.append('}')
    }
    else if (c eq classOf[Array[Int]]) {
      str.append("new int[] {")
      val arr: Array[Int] = o.asInstanceOf[Array[Int]]

      {
        var i: Int = 0
        while (i < arr.length) {
          {
            if (i > 0) str.append(", ")
            str.append(arr(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      str.append('}')
    }
    else if (c eq classOf[Array[Long]]) {
      str.append("new long[] {")
      val arr: Array[Long] = o.asInstanceOf[Array[Long]]

      {
        var i: Int = 0
        while (i < arr.length) {
          {
            if (i > 0) str.append(", ")
            str.append(arr(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      str.append('}')
    }
    else if (c eq classOf[Array[Float]]) {
      str.append("new float[] {")
      val arr: Array[Float] = o.asInstanceOf[Array[Float]]

      {
        var i: Int = 0
        while (i < arr.length) {
          {
            if (i > 0) str.append(", ")
            str.append(arr(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      str.append('}')
    }
    else if (c eq classOf[Array[Double]]) {
      str.append("new double[] {")
      val arr: Array[Double] = o.asInstanceOf[Array[Double]]

      {
        var i: Int = 0
        while (i < arr.length) {
          {
            if (i > 0) str.append(", ")
            str.append(arr(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      str.append('}')
    }
    else if (c eq classOf[Array[String]]) {
      str.append("new String[] {")
      val arr: Array[String] = o.asInstanceOf[Array[String]]

      {
        var i: Int = 0
        while (i < arr.length) {
          {
            if (i > 0) str.append(", ")
            formatValue(str, arr(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      str.append('}')
    }
    else if (o.isInstanceOf[Type]) {
      str.append("typeof(")
      str.append(o)
      str.append(")")
    }
    else str.append(o)
  }

  /**
   * Represents named arguments (assigned outside of the constructor) of a
   * custom attribute
   *
   * @param designator Designates if the named argument corresponds to a field or property.
   *                   Possible values: Signature.X_ELEMENT_KIND_FIELD = 0x53
   *                   Signature.X_ELEMENT_KIND_PROPERTY = 0x54
   * @param name       The name of the field/property.
   * @param `type`     Type of the field/property.
   * @param value      The value for the field/property.
   */
  class NamedArgument(val designator: Int, val name: String, val `type`: Type, val value: AnyRef) {
    /**
     * @return <b>true</b> if the named argument specifies a field;
     *         <b>false<b> otherwise.
     */
    def isField: Boolean = designator == Signature.X_ELEMENT_KIND_FIELD

    /**
     * @return <b>true</b> if the named argument specifies a property;
     *         <b>false<b> otherwise.
     */
    def isProperty: Boolean = designator == Signature.X_ELEMENT_KIND_PROPERTY

    /** @return a string representation of the named argument. */
    override def toString: String = {
      val str: StringBuffer = new StringBuffer(name)
      str.append(" = ")
      if (`type`.isEnum) str.append('(').append(`type`.fullName).append(')')
      formatValue(str, value)
      return str.toString
    }
  }

  class BoxedArgument(val `type`: Type, val value: AnyRef) {
    override def toString: String = s"(${`type`.fullName})$value"
  }
}

class Attribute(val constr: ConstructorInfo, val value: Array[Byte]) {
    assert(constr != null)
    assert(value != null, constr.toString)

  /** @return the type (class) of the attribute. */
  def getType: Type = constr.declaringType

  /** @return the constructor of this attribute. */
  def getConstructor: ConstructorInfo = constr

  /** @return the Blob with serialized constructor & named arguments. */
  def getValue: Array[Byte] = {
    val value: Array[Byte] = new Array[Byte](this.value.length)
    System.arraycopy(this.value, 0, value, 0, value.length)
    return value
  }

  /** @return an array with the arguments to the attribute's constructor. */
  def getConstructorArguments: Array[AnyRef] = {
    parseBlob
    val cas: Array[AnyRef] = new Array[AnyRef](constrArgs.length)
    System.arraycopy(constrArgs, 0, cas, 0, cas.length)
    return cas
  }

  /** @return the named argument with the given name. */
  def getNamedArgument(name: String): Attribute.NamedArgument = namedArgs.get(name)

  /** @return an array of all named arguments for this attribute. */
  def getNamedArguments: Array[Attribute.NamedArgument] =
    namedArgs.values.toArray(Array.empty[NamedArgument])

  /** @return a string representation of this attribute. */
  override def toString: String = {
    parseBlob
    val params: Array[ParameterInfo] = constr.getParameters
    assert(params.length == constrArgs.length, this.constr)
    val str: StringBuffer = new StringBuffer
    str.append('[')
    str.append(constr.declaringType.fullName)
    str.append('(')

    {
      var i: Int = 0
      while (i < constrArgs.length) {
        {
          if (i > 0) str.append(", ")
          val t: Type = params(i).parameterType
          if (t.isEnum) {
            str.append('(')
            str.append(t.fullName)
            str.append(')')
          }
          Attribute.formatValue(str, constrArgs(i))
        }
        ({
          i += 1; i - 1
        })
      }
    }
    val nargs: Array[Attribute.NamedArgument] = getNamedArguments

    {
      var i: Int = 0
      while (i < nargs.length) {
        {
          str.append(", ").append(nargs(i))
        }
        ({
          i += 1; i - 1
        })
      }
    }
    str.append(")]")
    return str.toString
  }

  private var constrArgs: Array[AnyRef] = null
  private var namedArgs: Map[String, Attribute.NamedArgument] = null
  private var buf: ByteBuffer = null

  private def parseBlob(): Unit = {
    try {
      parseBlob0
    } catch {
      case e: RuntimeException =>
        throw new RuntimeException(PEFile.bytes2hex(value), e)
    }
  }

  private def parseBlob0(): Unit = {
    if (buf != null) return
    buf = ByteBuffer.wrap(value)
    buf.order(ByteOrder.LITTLE_ENDIAN)
    val sig: Short = buf.getShort
    assert(sig == 1, PEFile.bytes2hex(value))
    val params: Array[ParameterInfo] = constr.getParameters
    constrArgs = new Array[AnyRef](params.length)

    {
      var i: Int = 0
      while (i < params.length) {
        {
          constrArgs(i) = parseFixedArg(params(i).parameterType)
        }
        ({
          i += 1; i - 1
        })
      }
    }
    val ncount: Int = buf.getShort
    namedArgs = new LinkedHashMap[String, Attribute.NamedArgument]
    {
      var i: Int = 0
      while (i < ncount) {
        {
          val designator: Int = buf.get
          assert(designator == Signature.X_ELEMENT_KIND_FIELD || designator == Signature.X_ELEMENT_KIND_PROPERTY, "0x" + PEFile.byte2hex(designator))
          val `type`: Type = parseFieldOrPropTypeInNamedArg
          val name: String = parseString
          val value: AnyRef = parseFixedArg(`type`)
          val narg: Attribute.NamedArgument = new Attribute.NamedArgument(designator, name, `type`, value)
          namedArgs.put(name, narg)
        }
        ({
          i += 1; i - 1
        })
      }
    }
  }

  private def parseFixedArg(typ: Type): AnyRef =
    if (typ.isArray) parseArray(typ.getElementType)
    else parseElem(typ)

  private def isSimpleElem(typ: Type): Boolean = {
    if (!Attribute.type2id.containsKey(typ)) return false
    val id: Int = Attribute.getTypeId(typ)
    id match {
      case Signature.ELEMENT_TYPE_STRING | Signature.X_ELEMENT_TYPE_TYPE | Signature.ELEMENT_TYPE_OBJECT =>
        return false
      case _ =>
        return true
    }
  }

  private def isStringElem(typ: Type): Boolean =
    if (!Attribute.type2id.containsKey(typ)) false
    else Attribute.getTypeId(typ) == Signature.ELEMENT_TYPE_STRING

  private def isTypeElem(typ: Type): Boolean =
    if (!Attribute.type2id.containsKey(typ)) false
    else Attribute.getTypeId(typ) == Signature.X_ELEMENT_TYPE_TYPE

  private def isSystemObject(typ: Type): Boolean =
    if (!Attribute.type2id.containsKey(typ)) false
    else Attribute.getTypeId(typ) == Signature.ELEMENT_TYPE_OBJECT

  private def parseElem(typ: Type): AnyRef = {
    if (isSimpleElem(typ)) return parseVal(Attribute.getTypeId(typ))
    if (typ.isEnum) return parseVal(Attribute.getTypeId(typ.getUnderlyingType))
    if (isStringElem(typ)) return parseString
    if (isTypeElem(typ)) return getTypeFromSerString
    if (isSystemObject(typ)) {
      val boxedT: Type = parse0x51
      if (boxedT.isEnum) {
        return new Attribute.BoxedArgument(boxedT, parseVal(Attribute.getTypeId(boxedT.getUnderlyingType)))
      }
      else {
        return new Attribute.BoxedArgument(boxedT, parseVal(Attribute.getTypeId(boxedT)))
      }
    }
    else {
      val boxedT: Type = parseType
      return parseVal(Attribute.getTypeId(boxedT))
    }
  }

  private def parseVal(id: Int): AnyRef = {
    id match {
      case Signature.ELEMENT_TYPE_BOOLEAN =>
        new java.lang.Boolean(if (buf.get == 0) false else true)
      case Signature.ELEMENT_TYPE_CHAR =>
        new java.lang.Character(buf.getChar)
      case Signature.ELEMENT_TYPE_I1 | Signature.ELEMENT_TYPE_U1 =>
        new java.lang.Byte(buf.get)
      case Signature.ELEMENT_TYPE_I2 | Signature.ELEMENT_TYPE_U2 =>
        new java.lang.Short(buf.getShort)
      case Signature.ELEMENT_TYPE_I4 | Signature.ELEMENT_TYPE_U4 =>
        new java.lang.Integer(buf.getInt)
      case Signature.ELEMENT_TYPE_I8 | Signature.ELEMENT_TYPE_U8 =>
        new java.lang.Long(buf.getLong)
      case Signature.ELEMENT_TYPE_R4 =>
        new java.lang.Float(buf.getFloat)
      case Signature.ELEMENT_TYPE_R8 =>
        new java.lang.Double(buf.getDouble)
      case Signature.X_ELEMENT_TYPE_TYPE =>
        getTypeFromSerString
      case Signature.ELEMENT_TYPE_STRING =>
        parseString
      case _ =>
        throw new RuntimeException("Shouldn't have called parseVal with: " + id)
    }
  }

  private def parseArray(typ: Type): AnyRef = {
    if (typ.isEnum) parseArray(typ.getUnderlyingType)
    else parseArray(Attribute.getTypeId(typ))
  }

  private def parseArray(id: Int): AnyRef =
    id match {
      case Signature.ELEMENT_TYPE_BOOLEAN =>
        parseBooleanArray
      case Signature.ELEMENT_TYPE_CHAR =>
        parseCharArray
      case Signature.ELEMENT_TYPE_I1 | Signature.ELEMENT_TYPE_U1 =>
        parseByteArray
      case Signature.ELEMENT_TYPE_I2 | Signature.ELEMENT_TYPE_U2 =>
        parseShortArray
      case Signature.ELEMENT_TYPE_I4 | Signature.ELEMENT_TYPE_U4 =>
        parseIntArray
      case Signature.ELEMENT_TYPE_I8 | Signature.ELEMENT_TYPE_U8 =>
        parseLongArray
      case Signature.ELEMENT_TYPE_R4 =>
        parseFloatArray
      case Signature.ELEMENT_TYPE_R8 =>
        parseDoubleArray
      case Signature.ELEMENT_TYPE_STRING =>
        parseStringArray
      case Signature.X_ELEMENT_TYPE_ENUM =>
        parseArray(getTypeFromSerString)
      case _ =>
        throw new RuntimeException("Unknown type id: " + id)
    }

  private def parseType: Type = {
    val id: Int = buf.get
    id match {
      case Signature.ELEMENT_TYPE_SZARRAY =>
        Type.mkArray(parseType, 1)
      case Signature.X_ELEMENT_TYPE_ENUM =>
        val enumName: String = parseString
        Type.getTypeInternal(enumName)
      case _ =>
        val t: Type = Attribute.id2type.get(new Integer(id))
        assert(t != null, PEFile.byte2hex(id))
        return t
    }
  }

  private def parse0x51: Type = {
    val id: Int = buf.get
    id match {
      case 0x51 =>
        return parse0x51
      case Signature.ELEMENT_TYPE_SZARRAY =>
        val arrT: Type = Type.mkArray(parseType, 1)
        return arrT
      case Signature.X_ELEMENT_TYPE_ENUM =>
        val enumName: String = parseString
        val enumT: Type = Type.getTypeInternal(enumName)
        return enumT
      case _ =>
        val t: Type = Attribute.id2type.get(new Integer(id))
        assert(t != null, PEFile.byte2hex(id))
        return t
    }
  }

  private def parseFieldOrPropTypeInNamedArg: Type = {
    val id: Int = buf.get
    id match {
      case 0x51 =>
        Attribute.id2type.get(new Integer(Signature.ELEMENT_TYPE_OBJECT))
      case Signature.X_ELEMENT_TYPE_ENUM =>
        val enumName: String = parseString
        Type.getTypeInternal(enumName)
      case _ =>
        val t: Type = Attribute.id2type.get(new Integer(id))
        assert(t != null, PEFile.byte2hex(id))
        return t
    }
  }

  private def getTypeFromSerString: Type = {
    val typename: String = parseString
    val i: Int = typename.indexOf(',')
    val name: String = if ((i < 0)) typename else typename.substring(0, i)
    var t: Type = Type.getType(name)
    if (t == null && i > 0) {
      val j: Int = typename.indexOf(',', i + 1)
      if (j > 0) {
        val assemName: String = typename.substring(i + 1, j)
        try {
          Assembly.loadFrom(assemName)
        }
        catch {
          case e: Throwable => {
            throw new RuntimeException(typename, e)
          }
        }
        t = Type.getType(name)
      }
    }
    assert(t != null, typename)
    return t
  }

  private def parseBooleanArray: Array[Boolean] = {
    val arr: Array[Boolean] = new Array[Boolean](buf.getInt)

    {
      var i: Int = 0
      while (i < arr.length) {
        arr(i) = if (buf.get == 0) false else true
        ({
          i += 1; i - 1
        })
      }
    }
    return arr
  }

  private def parseCharArray: Array[Char] = {
    val arr: Array[Char] = new Array[Char](buf.getInt)

    {
      var i: Int = 0
      while (i < arr.length) {
        arr(i) = buf.getChar
        ({
          i += 1; i - 1
        })
      }
    }
    return arr
  }

  private def parseByteArray: Array[Byte] = {
    val arr: Array[Byte] = new Array[Byte](buf.getInt)

    {
      var i: Int = 0
      while (i < arr.length) {
        arr(i) = buf.get
        ({
          i += 1; i - 1
        })
      }
    }
    return arr
  }

  private def parseShortArray: Array[Short] = {
    val arr: Array[Short] = new Array[Short](buf.getInt)

    {
      var i: Int = 0
      while (i < arr.length) {
        arr(i) = buf.getShort
        ({
          i += 1; i - 1
        })
      }
    }
    return arr
  }

  private def parseIntArray: Array[Int] = {
    val arr: Array[Int] = new Array[Int](buf.getInt)

    {
      var i: Int = 0
      while (i < arr.length) {
        arr(i) = buf.getInt
        ({
          i += 1; i - 1
        })
      }
    }
    return arr
  }

  private def parseLongArray: Array[Long] = {
    val arr: Array[Long] = new Array[Long](buf.getInt)

    {
      var i: Int = 0
      while (i < arr.length) {
        arr(i) = buf.getLong
        ({
          i += 1; i - 1
        })
      }
    }
    return arr
  }

  private def parseFloatArray: Array[Float] = {
    val arr: Array[Float] = new Array[Float](buf.getInt)

    {
      var i: Int = 0
      while (i < arr.length) {
        arr(i) = buf.getFloat
        ({
          i += 1; i - 1
        })
      }
    }
    return arr
  }

  private def parseDoubleArray: Array[Double] = {
    val arr: Array[Double] = new Array[Double](buf.getInt)

    {
      var i: Int = 0
      while (i < arr.length) {
        arr(i) = buf.getDouble
        ({
          i += 1; i - 1
        })
      }
    }
    return arr
  }

  private def parseStringArray: Array[String] = {
    val arr: Array[String] = new Array[String](buf.getInt)

    {
      var i: Int = 0
      while (i < arr.length) {
        arr(i) = parseString
        ({
          i += 1; i - 1
        })
      }
    }
    return arr
  }

  private def parseString: String = {
    var str: String = null
    val length: Int = parseLength
    if (length < 0) return null
    try {
      str = new String(value, buf.position, length, "UTF-8")
    }
    catch {
      case e: UnsupportedEncodingException => {
        throw new Error(e)
      }
    }
    buf.position(buf.position + length)
    return str
  }

  private def getByte: Int = {
    return (buf.get + 0x0100) & 0xff
  }

  def parseLength: Int = {
    var length: Int = getByte
    if ((length & 0xe0) == 0xe0) return -1
    if ((length & 0x80) != 0) {
      length = ((length & 0x7f) << 8) | getByte
      if ((length & 0x4000) != 0) length = ((length & 0x3fff) << 16) | (getByte << 8) | getByte
    }
    return length
  }
}