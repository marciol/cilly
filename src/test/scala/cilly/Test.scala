package cilly

import java.io.PrintStream

object Test {
  def main(args: Array[String]): Unit = {
    if (args == null || args.length < 1) {
      System.err.println("You must supply a filename!")
      System.exit(1)
    }
    val assem: Assembly = Assembly.loadFrom(args(0))
    Type.initMSCORLIB(assem)
    if (args.length >= 2) {
      val t: Type = Type.getType(args(1))
      if (t != null) dumpType(System.out, t)
      else System.err.println("Type " + args(1) + " not found!")
    }
    else {
      dumpAssembly(assem)
    }
  }

  def dumpAssembly(assem: Assembly): Unit = {
    val modules: Array[Module] = assem.modules
    val types: Array[Type] = modules(0).getTypes
      var i: Int = 0
      while (i < types.length) {
          System.out.println("#" + i + " -> " + types(i))
          i += 1
      }
  }

  def dumpType(out: PrintStream, typ: Type): Unit = {
    out.println("Type = " + typ)
    out.println("Name = " + typ.name)
    out.println("Namespace = " + typ.namespace)
    out.println("FullName = " + typ.fullName)
    out.println("Attributes = " + TypeAttributes.toString(typ.attributes))
    out.println("BaseType = " + typ.BaseType)
    val ifaces: Array[Type] = typ.getInterfaces
    if (ifaces != null) {
        var i: Int = 0
        while (i < ifaces.length) {
          out.println("\timplements " + ifaces(i))
            i += 1
        }
    }
    out.println("Assembly = " + typ.assembly)
    out.println("Module = " + typ.module)
    out.println("DeclaringType = " + typ.declaringType)
    out.println("IsInterface = " + typ.isInterface)
    out.println("IsAbstract = " + typ.isAbstract)
    val fields: Array[FieldInfo] = typ.getFields(BindingFlags.Instance | BindingFlags.Static | BindingFlags.NonPublic)
    out.println("\nFields (" + fields.length + "):")

    {
      var i: Int = 0
      while (i < fields.length) {
          out.println("\t" + fields(i))
          out.println("\t\tDeclaringType = " + fields(i).declaringType)
          i += 1
      }
    }
    val constrs: Array[ConstructorInfo] = typ.getConstructorsArray
    out.println("\nConstructors (" + constrs.length + "):")

    {
      var i: Int = 0
      while (i < constrs.length) {
        {
          out.println("\t" + constrs(i))
        }
        ({
          i += 1; i - 1
        })
      }
    }
    val methods: Array[MethodInfo] = typ.getMethodsArray
    out.println("\nMethods (" + methods.length + "):")

    {
      var i: Int = 0
      while (i < methods.length) {
        {
          out.println("\t" + methods(i))
          out.println("\t\tDeclaringType = " + methods(i).declaringType)
        }
        ({
          i += 1
        })
      }
    }
  }
}