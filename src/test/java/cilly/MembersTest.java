package cilly;

import java.io.PrintStream;

public class MembersTest {

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("usage: java test.MembersTest assembly [classname]");
            System.exit(1);
        }

        Assembly mscorlib = Assembly.loadFrom("/usr/lib/mono/2.0/mscorlib.dll");
        Type.initMSCORLIB(mscorlib);
        Assembly assem = Assembly.loadFrom(args[0]);
        if (args.length > 1) {
            Type type = assem.getType(args[1]);
            if (type != null)
                dumpMember(System.out, type);
            else
                System.err.println("Cannot find type " + args[1] + " in "
                        + assem);
        } else {
            Type[] types = assem.getTypes();
            System.out.println("Number of types in assembly " + assem + " -> "
                    + types.length);
            dumpCustomAttributes(System.out, "assembly: ", assem);
            Module[] modules = assem.getModules();
            for (int i = 0; i < modules.length; i++) {
                dumpCustomAttributes(System.out, "module " + modules[i] + ": ",
                        modules[i]);
            }
            dumpMembers(System.out, types);
        }
    }

    public static final void dumpMember(PrintStream out, MemberInfo member) {
        try {
            if (member.memberType() == MemberTypes.TypeInfo
                    || member.memberType() == MemberTypes.NestedType) {
                Type type = (Type) member;
                dumpCustomAttributes(out, "", type);
                out.print(TypeAttributes.accessModsToString(type.Attributes));
                out.print(type.isInterface() ? " interface " : " class ");
                out.print(type);
                if (type.BaseType() != null)
                    out.println(" extends " + type.BaseType());
                Type[] ifaces = type.getInterfaces();
                if (ifaces.length > 0) {
                    out.print("\timplements ");
                    for (int i = 0; i < ifaces.length; i++) {
                        out.print(ifaces[i]);
                        if (i < (ifaces.length - 1))
                            out.print(", ");
                    }
                    out.println();
                }
                out.println("{");
                int all = BindingFlags.Public | BindingFlags.DeclaredOnly// |
                                                                         // BindingFlags.NonPublic
                        | BindingFlags.Instance | BindingFlags.Static;
                dumpMembers(out, type.getNestedTypes());
                dumpMembers(out, type.getFields(all));
                dumpMembers(out, type.getConstructors(all));
                dumpMembers(out, type.getMethods(all));
                dumpMembers(out, type.getProperties(all));
                dumpMembers(out, type.getEvents());
                out.println("}");
            } else {
                dumpCustomAttributes(out, "", member);
                out.print(MemberTypes.toString(member.memberType()));
                out.print(": ");
                out.print(member);
                out.println();
            }
        } catch (Throwable e) {
            String message = MemberTypes.toString(member.memberType()) + ": "
                    + member;
            throw new RuntimeException(message, e);
        }
    }

    public static void dumpCustomAttributes(PrintStream out, String prefix,
            ICustomAttributeProvider att) {
        Object[] attrs = att.getCustomAttributes(false);
        for (int j = 0; j < attrs.length; j++)
            out.println(prefix + attrs[j]);
    }

    public static void dumpMembers(PrintStream out, MemberInfo[] members) {
        for (int i = 0; i < members.length; i++) {
            dumpMember(out, members[i]);
        }
    }

}
