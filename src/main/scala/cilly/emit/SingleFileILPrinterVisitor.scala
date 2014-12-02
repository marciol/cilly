/*
 * System.Reflection.Emit-like API for writing .NET assemblies in MSIL
 */

package cilly.emit

import java.io.FileWriter
import java.io.BufferedWriter
import java.io.PrintWriter
import java.io.IOException
import java.util.Iterator
import java.util.HashMap
import java.util.Arrays

import cilly._
import cilly.util.Table

/**
 * The MSIL printer Visitor. It prints a complete
 * assembly in a single file that can be compiled by ilasm.
 *
 * @author Nikolay Mihaylov
 * @author Daniel Lorch
 * @version 1.0
 */
final class SingleFileILPrinterVisitor(_fileName: String) extends ILPrinterVisitor {
  var fileName: String = _fileName

  out = new PrintWriter(new BufferedWriter(new FileWriter(fileName)))

  /**
   * Visit an AssemblyBuilder
   */
  @throws(classOf[IOException])
  def caseAssemblyBuilder(assemblyBuilder: AssemblyBuilder): Unit = {
    ILPrinterVisitor.currAssembly = assemblyBuilder

    // first get the entryPoint
    this.entryPoint = assemblyBuilder.EntryPoint

    // all external assemblies
    as = assemblyBuilder.getExternAssemblies()
    scala.util.Sorting.quickSort(as)(assemblyNameComparator) // Arrays.sort(as, assemblyNameComparator)

    assemblyBuilder.generatedFiles += fileName
    printAssemblyBoilerplate()

    // print each module
    val m: Array[Module] = assemblyBuilder.getModules()
    nomembers = true
    for (i <- 0 until m.length) {
      print(m(i).asInstanceOf[ModuleBuilder])
    }

    nomembers = false
    for (i <- 0 until m.length) {
      print(m(i).asInstanceOf[ModuleBuilder])
    }
    // close out file
    out.close()
    ILPrinterVisitor.currAssembly = null
  }

  /**
   * Visit a ModuleBuilder
   */
  @throws(classOf[IOException])
  def caseModuleBuilder(module: ModuleBuilder): Unit = {
    // print module declaration
    currentModule = module
    if (nomembers) {
      print(".module \'"); print(module.name); println("\'")
      printAttributes(module)
    }

    if (!module.globalsCreated)
      module.CreateGlobalFunctions()

    val m: Array[MethodInfo] = module.getMethods()
    for (i <- 0 until m.length) {
      print(m(i).asInstanceOf[MethodBuilder])
    }

    val t: Array[Type] = module.getTypes()
    for (i <- 0 until t.length) {
      print(t(i).asInstanceOf[TypeBuilder])
    }
    currentModule = null
  }

}  // class SingleFileILPrinterVisitor
