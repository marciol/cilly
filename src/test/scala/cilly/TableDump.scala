package cilly

import cilly.util.Table
import cilly.util.Table._
import java.io.PrintStream
import java.io.FileNotFoundException

object TableDump {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("You must supply a filename!")
      System.exit(1)
    }
    var file: TableDump = null
    try {
      file = new TableDump(args(0))
    }
    catch {
      case e: FileNotFoundException =>
        e.printStackTrace()
    }
    if (args.length > 1) {
        var i: Int = 1
        var found = false
        while (i < args.length) {
            val name: String = args(i)
            found = false
              var tableId: Int = 0
              while (!found && tableId < Table.MAX_NUMBER) {
                  val table: Table = file.getTable(tableId)
                  if ((table.rows > 0) && (name == table.getTableName)) {
                    file.dump(System.out, table)
                    System.out.println()
                    found = true
                  }
                tableId += 1
              }
            System.err.println("No such table: " + name)
            i += 1
        }
    }
    else file.dump(System.out)
  }
}

class TableDump(filename: String) extends PEFile(filename) {
  import PEFile._
  /** */
  def dump(out: PrintStream): Unit = {
    out.println("CLI RVA: " + CLI_RVA)
    out.println("Optional header size: " + optHeaderSize)
    out.println("Number of sections: " + numOfSections)
    out.println
    {
      var i: Int = 0
      while (i < sections.length) {
        {
          sections(i).dump(out)
          out.println()
        }
        ({
          i += 1; i - 1
        })
      }
    }
    out.println("MetaData Offset:   0x" + Integer.toHexString(posMetadata))
    out.println("Number of streams: " + numOfStreams)
    out.println("#~ stream")
    Meta.dump(out)
    out.println()
    out.println("#Strings stream")
    Strings.dump(out)
    out.println()
    if (US != null) {
      out.println("#US stream")
      US.dump(out)
      out.println()
    }
    out.println("#GUID stream")
    GUID.dump(out)
    out.println()
    out.println("#Blob stream")
    Blob.dump(out)
    out.println()
    out.println("Heap Sizes IndexedSeq = 0x0" + Integer.toHexString(heapSizes))
    out.println
    {
      var i: Int = 0
      while (i < Table.MAX_NUMBER) {
        if (getTable(i).rows > 0) {
          dump(out, getTable(i))
          out.println()
        }
        ({
          i += 1; i - 1
        })
      }
    }
  }

  /** Dumps the contents of this table. */
  def dump(out: PrintStream, table: Table): Unit = {
    out.println("Table:" + "  ID = 0x" + byte2hex(table.id))
    out.println("\tname = " + table.getTableName)
    out.println("\trows =  " + table.rows)

    var i: Int = 1
    while (i <= table.rows) {
      dumpRow(out, table, i)
      i += 1
    }
  }

  def dumpIndex(out: PrintStream, tableSetId: Int, index: Int): Unit = {
    val tableId: Int = Table.tableId(tableSetId, index)
    val row: Int = Table.tableIndex(tableSetId, index)
    out.print(getTable(tableId).getTableName)
    out.print('[')
    out.print(if (getTable(tableId).isShort) short2hex(row) else int2hex(row))
    out.print(']')
  }

  def dumpRow(out: PrintStream, table: Table, row: Int): Unit = {
    table.readRow(row)
    out.print(table.getTableName)
    out.print("[" + short2hex(row) + "]: ")
    dumpRow(out, table)
    out.println()
  }

  /** Prints the current content of the fields of the class. */
  def dumpRow(out: PrintStream, table: Table): Unit = {
    if (table.isInstanceOf[Table.ModuleDef]) {
      val t: Table.ModuleDef = table.asInstanceOf[Table.ModuleDef]
      out.print("Generation = 0x" + short2hex(t.Generation))
      out.print("; Name = " + getString(t.Name))
    }
    else if (table.isInstanceOf[Table.TypeRef]) {
      val t: Table.TypeRef = table.asInstanceOf[Table.TypeRef]
      out.print("FullName = " + t.getFullName)
      out.print("; ResolutionScope = 0x" + int2hex(t.ResolutionScope))
    }
    else if (table.isInstanceOf[Table.TypeDef]) {
      val t: Table.TypeDef = table.asInstanceOf[Table.TypeDef]
      out.print("Flags = 0x")
      out.print(int2hex(t.Flags))
      out.print("; FullName = ")
      out.print(t.getFullName)
      out.print("; Extends = ")
      dumpIndex(out, Table._TypeDefOrRef, t.Extends)
      out.print("; FieldList = ")
      out.print(t.FieldList)
      out.print("; MethodList = ")
      out.print(t.MethodList)
    }
    else if (table.isInstanceOf[Table.FieldTrans]) {
      val t: Table.FieldTrans = table.asInstanceOf[Table.FieldTrans]
      out.print("Field = ")
      out.print(t.Field)
    }
    else if (table.isInstanceOf[Table.FieldDef]) {
      val t: Table.FieldDef = table.asInstanceOf[Table.FieldDef]
      out.print("Flags = 0x" + short2hex(t.Flags))
      out.print("; Name = " + t.getName)
      out.print("; Signature = (" + bytes2hex(getBlob(t.Signature)) + ")")
    }
    else if (table.isInstanceOf[Table.MethodTrans]) {
      val t: Table.MethodTrans = table.asInstanceOf[Table.MethodTrans]
      out.print("Method = ")
      out.print(t.Method)
    }
    else if (table.isInstanceOf[Table.MethodDef]) {
      val t: Table.MethodDef = table.asInstanceOf[Table.MethodDef]
      out.print("Flags = 0x" + short2hex(t.Flags))
      out.print("; Name = " + t.getName)
      out.print("; ParamList = " + t.ParamList)
      out.print("; Signature = (" + bytes2hex(getBlob(t.Signature)) + ")")
    }
    else if (table.isInstanceOf[Table.ParamDef]) {
      val t: Table.ParamDef = table.asInstanceOf[Table.ParamDef]
      out.print("Flags = 0x" + short2hex(t.Flags))
      out.print("; Name = " + t.getName)
      out.print("; Sequence = " + t.Sequence)
    }
    else if (table.isInstanceOf[Table.InterfaceImpl]) {
      val t: Table.InterfaceImpl = table.asInstanceOf[Table.InterfaceImpl]
      out.print("Class = 0x" + short2hex(t.Class))
      out.print("; Interface = 0x" + short2hex(t.Interface))
    }
    else if (table.isInstanceOf[Table.MemberRef]) {
      val t: Table.MemberRef = table.asInstanceOf[Table.MemberRef]
      out.print("Name = " + t.getName)
      out.print("; Signature = (" + bytes2hex(getBlob(t.Signature)) + ")")
      out.print("; Class = " + t.Class)
    }
    else if (table.isInstanceOf[Table.Constant]) {
      val t: Table.Constant = table.asInstanceOf[Table.Constant]
      out.print("Parent = ")
      dumpIndex(out, Table._HasConstant, t.Parent)
      out.print("; Type = 0x" + byte2hex(t.Type))
      out.print("; Value = (" + bytes2hex(getBlob(t.Value)))
      out.print("); Value = " + t.getValue)
    }
    else if (table.isInstanceOf[Table.CustomAttribute]) {
      val t: Table.CustomAttribute = table.asInstanceOf[Table.CustomAttribute]
      out.print("Parent = ")
      dumpIndex(out, Table._HasCustomAttribute, t.Parent)
      out.print("; Type = ")
      dumpIndex(out, Table._CustomAttributeType, t.Type)
      out.print("; Value = (" + bytes2hex(t.getValue) + ")")
    }
    else if (table.isInstanceOf[Table.FieldMarshal]) {
      val t: Table.FieldMarshal = table.asInstanceOf[Table.FieldMarshal]
      out.print("NativeType = (")
      out.print(bytes2hex(getBlob(t.NativeType)) + ")")
    }
    else if (table.isInstanceOf[Table.DeclSecurity]) {
      val t: Table.DeclSecurity = table.asInstanceOf[Table.DeclSecurity]
      out.print("Action = 0x" + short2hex(t.Action))
      out.print("; PermissionSet = (" + bytes2hex(getBlob(t.PermissionSet)) + ")")
    }
    else if (table.isInstanceOf[Table.ClassLayout]) {
      val t: Table.ClassLayout = table.asInstanceOf[Table.ClassLayout]
      out.print("PackingSize = 0x" + short2hex(t.PackingSize))
      out.print("; ClassSize = 0x" + int2hex(t.ClassSize))
      out.print(": Parent = " + t.Parent + " (ref to: ")
      dumpRow(out, this.typeDef(t.Parent))
      out.print(")")
    }
    else if (table.isInstanceOf[Table.FieldLayout]) {
      val t: Table.FieldLayout = table.asInstanceOf[Table.FieldLayout]
      out.print("Offset = 0x" + int2hex(t.Offset))
      out.print("; Field = (ref to: ")
      dumpRow(out, this.fieldDef(t.Field))
      out.print(")")
    }
    else if (table.isInstanceOf[Table.StandAloneSig]) {
      val t: Table.StandAloneSig = table.asInstanceOf[Table.StandAloneSig]
      out.print("StandAloneSig: Signature = (" + bytes2hex(getBlob(t.Signature)) + ")")
    }
    else if (table.isInstanceOf[Table.EventMap]) {
      val t: Table.EventMap = table.asInstanceOf[Table.EventMap]
      out.print("Parent = 0x" + int2hex(t.Parent) + " (ref to: ")
      dumpRow(out, this.typeDef(t.Parent))
      out.print("); EventList = 0x")
      out.print(int2hex(t.EventList))
    }
    else if (table.isInstanceOf[Table.EventDef]) {
      val t: Table.EventDef = table.asInstanceOf[Table.EventDef]
      out.print("EventFlags = 0x" + short2hex(t.EventFlags))
      out.print("; Name = " + t.getName)
      out.print("; EventType = 0x" + int2hex(t.EventType))
    }
    else if (table.isInstanceOf[Table.PropertyMap]) {
      val t: Table.PropertyMap = table.asInstanceOf[Table.PropertyMap]
      out.print("Parent = " + t.Parent + " (ref to: ")
      dumpRow(out, this.typeDef(t.Parent))
      out.print(")")
    }
    else if (table.isInstanceOf[Table.PropertyDef]) {
      val t: Table.PropertyDef = table.asInstanceOf[Table.PropertyDef]
      out.print("Flags = 0x" + short2hex(t.Flags))
      out.print("; Name = " + t.getName)
      out.print("; Type = (" + bytes2hex(getBlob(t.Type)) + ")")
    }
    else if (table.isInstanceOf[Table.MethodSemantics]) {
      val t: Table.MethodSemantics = table.asInstanceOf[Table.MethodSemantics]
      out.print("Semantics = 0x" + short2hex(t.Semantics))
      out.print("; Method = 0x" + int2hex(t.Method) + " (ref to: ")
      dumpRow(out, this.methodDef(t.Method))
      out.print("); Association = 0x" + int2hex(t.Association))
    }
    else if (table.isInstanceOf[Table.MethodImpl]) {
      val t: Table.MethodImpl = table.asInstanceOf[Table.MethodImpl]
      out.print("Class = (ref to: ")
      dumpRow(out, this.typeDef(t.Class))
      out.print(")")
    }
    else if (table.isInstanceOf[Table.ModuleRef]) {
      val t: Table.ModuleRef = table.asInstanceOf[Table.ModuleRef]
      out.print("Name = " + t.getName)
    }
    else if (table.isInstanceOf[Table.TypeSpec]) {
      val t: Table.TypeSpec = table.asInstanceOf[Table.TypeSpec]
      out.print("Signature = (" + bytes2hex(getBlob(t.Signature)) + ")")
    }
    else if (table.isInstanceOf[Table.ImplMap]) {
      val t: Table.ImplMap = table.asInstanceOf[Table.ImplMap]
      out.print("ImportName = " + getString(t.importName))
    }
    else if (table.isInstanceOf[Table.FieldRVA]) {
      val t: Table.FieldRVA = table.asInstanceOf[Table.FieldRVA]
      out.print("RVA = 0x" + int2hex(t.RVA))
      out.print("; Field = (ref to: ")
      dumpRow(out, this.fieldDef(t.Field))
      out.print(")")
    }
    else if (table.isInstanceOf[Table.AssemblyDef]) {
      val t: Table.AssemblyDef = table.asInstanceOf[Table.AssemblyDef]
      out.print("Flags = 0x" + int2hex(t.Flags))
      out.print(" ; Name = " + getString(t.Name))
      out.print("; Culture = " + getString(t.Culture))
      out.print(" ; Version = " + t.MajorVersion + ".")
      out.print(t.MinorVersion + "." + t.BuildNumber)
      out.print("." + t.RevisionNumber)
      out.print("; HashAlgId = 0x" + int2hex(t.HashAlgId))
      out.print("; PublicKey = (")
      out.print(bytes2hex(getBlob(t.PublicKey)) + ")")
    }
    else if (table.isInstanceOf[Table.AssemblyProcessor]) {
      val t: Table.AssemblyProcessor = table.asInstanceOf[Table.AssemblyProcessor]
      out.print("Processor = 0x" + int2hex(t.Processor))
    }
    else if (table.isInstanceOf[Table.AssemblyOS]) {
      val t: Table.AssemblyOS = table.asInstanceOf[Table.AssemblyOS]
      out.print("!?!")
    }
    else if (table.isInstanceOf[Table.AssemblyRef]) {
      val t: Table.AssemblyRef = table.asInstanceOf[Table.AssemblyRef]
      out.print("Flags = 0x" + int2hex(t.Flags))
      out.print("; Name = " + getString(t.Name))
      out.print("; Culture = " + getString(t.Culture))
      out.print("; Version = " + t.MajorVersion + "." + t.MinorVersion)
      out.print("." + t.BuildNumber + "." + t.RevisionNumber)
      out.print("; PublicKeyOrToken = (" + bytes2hex(getBlob(t.PublicKeyOrToken)) + ")")
      out.print("; HashValue = (" + bytes2hex(getBlob(t.HashValue)) + ")")
    }
    else if (table.isInstanceOf[Table.AssemblyRefProcessor]) {
      val t: Table.AssemblyRefProcessor = table.asInstanceOf[Table.AssemblyRefProcessor]
      out.print("!?!")
    }
    else if (table.isInstanceOf[Table.AssemblyRefOS]) {
      val t: Table.AssemblyRefOS = table.asInstanceOf[Table.AssemblyRefOS]
      out.print("!?!")
    }
    else if (table.isInstanceOf[Table.FileDef]) {
      val t: Table.FileDef = table.asInstanceOf[Table.FileDef]
      out.print("Flags = 0x" + int2hex(t.Flags))
      out.print("; Name = " + t.getName)
      out.print("; HashValue = (" + bytes2hex(getBlob(t.HashValue)) + ")")
    }
    else if (table.isInstanceOf[Table.ExportedType]) {
      val t: Table.ExportedType = table.asInstanceOf[Table.ExportedType]
      out.print("FullName = " + t.getFullName)
    }
    else if (table.isInstanceOf[Table.ManifestResource]) {
      val t: Table.ManifestResource = table.asInstanceOf[Table.ManifestResource]
      out.print("Name = " + getString(t.Name))
      out.print("; Flags = 0x" + int2hex(t.Flags))
    }
    else if (table.isInstanceOf[Table.NestedClass]) {
      val t: Table.NestedClass = table.asInstanceOf[Table.NestedClass]
      out.print(this.typeDef(t.enclosingClass).getFullName)
      out.print("/")
      out.print(this.typeDef(t.nestedClass).getFullName)
    }
    else if (table.isInstanceOf[Table.GenericParam]) {
      val t: Table.GenericParam = table.asInstanceOf[Table.GenericParam]
      out.print(getString(t.name))
    }
    else if (table.isInstanceOf[Table.MethodSpec]) {
      val t: Table.MethodSpec = table.asInstanceOf[Table.MethodSpec]
      out.print(bytes2hex(getBlob(t.instantiation)))
    }
    else if (table.isInstanceOf[Table.GenericParamConstraint]) {
      val t: Table.GenericParamConstraint = table.asInstanceOf[Table.GenericParamConstraint]
    }
    else {
      throw new RuntimeException("Unknown table " + table.getClass)
    }
  }
}