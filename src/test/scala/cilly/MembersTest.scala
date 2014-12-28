package cilly

import java.io.PrintStream

object MembersTest {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("usage: java test.MembersTest assembly [classname]")
      System.exit(1)
    }
    val mscorlib: Assembly = Assembly.loadFrom("/usr/lib/mono/2.0/mscorlib.dll")
    Type.initMSCORLIB(mscorlib)
    val assem: Assembly = Assembly.loadFrom(args(0))
    if (args.length > 1) {
      val `type`: Type = assem.getType(args(1))
      if (`type` != null) dumpMember(System.out, `type`)
      else System.err.println("Cannot find type " + args(1) + " in " + assem)
    }
    else {
      val types: Array[Type] = assem.getTypes
      System.out.println("Number of types in assembly " + assem + " -> " + types.length)
      dumpCustomAttributes(System.out, "assembly: ", assem)
      val modules: Array[Module] = assem.modules
      
      {
        var i: Int = 0
        while (i < modules.length) {
          {
            dumpCustomAttributes(System.out, "module " + modules(i) + ": ", modules(i))
          }
          ({
            i += 1; i - 1
          })
        }
      }
      dumpMembers(System.out, types)
    }
  }

  def dumpMember(out: PrintStream, member: MemberInfo): Unit = {
    try {
      if (member.memberType == MemberTypes.TypeInfo || member.memberType == MemberTypes.NestedType) {
        val typ: Type = member.asInstanceOf[Type]
        dumpCustomAttributes(out, "", typ)
        out.print(TypeAttributes.accessModsToString(typ.attributes))
        out.print(if (typ.isInterface) " interface " else " class ")
        out.print(typ)
        if (typ.BaseType != null) out.println(" extends " + typ.BaseType)
        val ifaces: Array[Type] = typ.getInterfaces
        if (ifaces.length > 0) {
          out.print("\timplements ")
          
          {
            var i: Int = 0
            while (i < ifaces.length) {
              {
                out.print(ifaces(i))
                if (i < (ifaces.length - 1)) out.print(", ")
              }
              ({
                i += 1; i - 1
              })
            }
          }
          out.println()
        }
        out.println("{")
        val all: Int = BindingFlags.Public | BindingFlags.DeclaredOnly | BindingFlags.Instance | BindingFlags.Static
        dumpMembers(out, typ.getNestedTypes)
        dumpMembers(out, typ.getFields(all))
        dumpMembers(out, typ.getConstructors(all))
        dumpMembers(out, typ.getMethods(all))
        dumpMembers(out, typ.getProperties(all))
        dumpMembers(out, typ.getEvents)
        out.println("}")
      }
      else {
        dumpCustomAttributes(out, "", member)
        out.print(MemberTypes.toString(member.memberType))
        out.print(": ")
        out.print(member)
        out.println()
      }
    }
    catch {
      case e: Throwable =>
        val message: String = MemberTypes.toString(member.memberType) + ": " + member
        throw new RuntimeException(message, e)
    }
  }

  def dumpCustomAttributes(out: PrintStream, prefix: String, att: ICustomAttributeProvider): Unit = {
    val attrs: Array[AnyRef] = att.getCustomAttributes(inherit = false)
    
    {
      var j: Int = 0
      while (j < attrs.length) {
        out.println(prefix + attrs(j))
        j += 1
      }
    }
  }

  def dumpMembers(out: PrintStream, members: Array[_ <: MemberInfo]): Unit = {
    {
      var i: Int = 0
      while (i < members.length) {
        {
          dumpMember(out, members(i))
        }
        ({
          i += 1; i - 1
        })
      }
    }
  }
}