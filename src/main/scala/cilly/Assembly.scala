/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

import java.io.{File, FileNotFoundException}
import java.util.{HashMap, Iterator}

import cilly.util.Table

/**
 * Defines an Assembly, which is a reusable, versionable, and self-describing
 * building block of a common language runtime application.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object Assembly {
  val assemblies: HashMap[String, Assembly] = new HashMap[String, Assembly]

  /** Loads an assembly from the specified path. */
  def loadFrom(assemblyFileName: String): Assembly = {
    val afile: File = new File(assemblyFileName)
    loadFrom(afile.getParentFile, afile.getName)
  }

  /** Loads an assembly with the given name from the given directory. */
  def loadFrom(dir: File, _name: String): Assembly = {
    var name = _name
    var file: File = null
    var pefile: PEFile = null
    if (name.toUpperCase.endsWith(".EXE") || name.toUpperCase.endsWith(".DLL")) {
      file = new File(dir, name)
      pefile = getPEFile(file)
      name = name.substring(0, name.length - 4)
    }
    val adir: File = if (pefile == null) new File(dir, name) else null
    if (pefile == null) {
      file = new File(dir, name + ".dll")
      pefile = getPEFile(file)
    }
    if (pefile == null) {
      file = new File(dir, name + ".DLL")
      pefile = getPEFile(file)
    }
    if (pefile == null && adir.exists) {
      file = new File(adir, name + ".dll")
      pefile = getPEFile(file)
    }
    if (pefile == null && adir.exists) {
      file = new File(adir, name + ".DLL")
      pefile = getPEFile(file)
    }
    if (pefile == null) {
      file = new File(dir, name + ".exe")
      pefile = getPEFile(file)
    }
    if (pefile == null) {
      file = new File(dir, name + ".EXE")
      pefile = getPEFile(file)
    }
    if (pefile == null && adir.exists) {
      file = new File(adir, name + ".exe")
      pefile = getPEFile(file)
    }
    if (pefile == null && adir.exists) {
      file = new File(adir, name + ".EXE")
      pefile = getPEFile(file)
    }
    if (pefile == null) throw new RuntimeException("Cannot find assembly " + new File(dir, name))
    getPEAssembly(pefile)
  }

  private def getPEAssembly(pefile: PEFile): Assembly = {
    val assem: Table.AssemblyDef = pefile.AssemblyDef
    if (assem == null) throw new RuntimeException("File " + pefile + " does not contain a manifest")
    assem.readRow(1)
    val name: String = pefile.getString(assem.Name)
    val a: Assembly = assemblies.get(name)
    if (a != null) {
      return a
    }
    val assemName: String = pefile.getString(assem.Name)
    val assemVersion: Version = new Version(assem.MajorVersion, assem.MinorVersion, assem.BuildNumber, assem.RevisionNumber)
    val an: AssemblyName = new AssemblyName(assemName, assemVersion)
    an.setPublicKey(pefile.getBlob(assem.PublicKey))
    new PEAssembly(pefile, an)
  }

  def getPEFile(f: File): PEFile = {
    try {
      new PEFile(f.getAbsolutePath)
    } catch {
        case e: FileNotFoundException => null
        case e: RuntimeException =>
          println("swallowed RuntimeException at getPEFile")
          e.printStackTrace()
          null
    }
  }

  def getAssembly(name: String): Assembly = assemblies.get(name)
}

abstract class Assembly(val assemblyName: AssemblyName, external: Boolean = false) extends CustomAttributeProvider {
  /** The entry point of this assembly. */
  var entryPoint: MethodInfo = null
  /** the display name of the assembly. */
  final val fullName: String = assemblyName.toString

  if (external)
      Assembly.assemblies.put(assemblyName.name, this)

  /** @return the file from which this assembly was loaded. */
  def file: File = throw new RuntimeException("Not supported")

  /** Gets the specified module in this assembly. Works on filenames. */
  def module(name: String): Module = {
    doInitModules()
    modulesMap.get(name)
  }

  /** Get all the modules of the assembly. */
  def modules: Array[Module] = {
    doInitModules()
    modulesMap.values.toArray(new Array[Module](modulesMap.size))
  }

  /** Get the corresponding type. */
  def getType(name: String): Type = {
    doInitModules()
    val modules: Iterator[Module] = modulesMap.values.iterator
    var t: Type = null
    while (t == null && modules.hasNext) {
      t = modules.next.getType(name)
    }
    t
  }

  /** @return an array of all types defined in the assembly. */
  def getTypes: Array[Type] = {
    if (types != null) return types.clone
    doInitModules()
    val modules: Iterator[Module] = modulesMap.values.iterator
    var newTypes: Array[Type] = modules.next.getTypes
    while (modules.hasNext) {
      val module: Module = modules.next
      val mtypes: Array[Type] = module.getTypes
      val oldTypes: Array[Type] = newTypes
      newTypes = new Array[Type](oldTypes.length + mtypes.length)
      System.arraycopy(oldTypes, 0, newTypes, 0, oldTypes.length)
      System.arraycopy(mtypes, 0, newTypes, oldTypes.length, mtypes.length)
    }
    types = newTypes
    types.clone
  }

  def getName: AssemblyName = assemblyName

  override def toString: String = fullName

  protected var types: Array[Type] = null
  private final val modulesMap: HashMap[String, Module] = new HashMap[String, Module]

  private[cilly] def addType(typ: Type): Unit = Type.addType(typ)

  protected def addModule(name: String, module: Module): Unit = modulesMap.put(name, module)

  private var initModules: Boolean = true

  protected final def doInitModules(): Unit = {
    if (initModules) {
      loadModules
      initModules = false
    }
  }

  /** used for lazy construction of the Assembly. */
  protected def loadModules(): Unit

  private[cilly] def dumpTypes(): Unit = {
    val types: Array[Type] = getTypes

    {
      var i: Int = 0
      while (i < types.length) {
        System.out.println(types(i))
        ({
          i += 1; i - 1
        })
      }
    }
  }
}