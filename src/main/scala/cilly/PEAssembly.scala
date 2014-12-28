/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.io.File

import cilly.util.Table

/** Represents an assembly that resides in a real .NET assembly
  *
  * @author Nikolay Mihaylov
  * @version 1.0
  */
final class PEAssembly(val pefile: PEFile, override val assemblyName: AssemblyName) extends Assembly(assemblyName, true) {

  private def name: String = pefile.moduleDef(1).name
  private final val mainModule: PEModule = new PEModule(pefile, 1, name, this)
  addModule(name, mainModule)

  protected def loadModules: Unit = {
    val parentDir: File = pefile.parentFile
    val fd: Table.FileDef = pefile.FileDef

    var row: Int = 1
    while (row <= fd.rows) {
      fd.readRow(row)
      val filename: String = fd.getName
      var f: File = new File(parentDir, filename)
      var pe: PEFile = Assembly.getPEFile(f)
      if (pe == null) {
        f = new File(filename)
        pe = Assembly.getPEFile(f)
      }
      if (pe != null) {
        val name: String = pe.moduleDef(1).name
        val module: PEModule = new PEModule(pe, 1, name, this)
        addModule(name, module)
      }
      row += 1
    }
  }

  override def file: File = pefile.underlyingFile

  protected override def loadCustomAttributes(attributeType: Type): Unit = {
    doInitModules()
    mainModule.initAttributes(this, 1, Table.AssemblyDef.ID, attributeType)
  }
}