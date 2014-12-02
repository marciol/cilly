package cilly;

import java.io.PrintStream;

public class Test {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("You must supply a filename!");
            System.exit(1);
        }

        Assembly assem = Assembly.loadFrom(args[0]);
        Type.initMSCORLIB(assem);

        // "System.Collections.ArrayList"
        if (args.length >= 2) {
            Type t = Type.GetType(args[1]);
            dumpType(System.out, t);
        } else {
            dumpAssembly(assem);
        }
    }

    public static void dumpAssembly(Assembly assem) {
        Module[] modules = assem.getModules();
        // System.out.println("Modules in assembly " + assem +
        // " (" + modules.length + ")");
        // for (int i = 0; i < modules.length; i++) {
        // System.out.println("\t" + modules[i]);
        // }

        Type[] types = modules[0].getTypes();
        // System.out.println("Types in assembly " + assem +
        // " (" + types.length + ")");
        for (int i = 0; i < types.length; i++) {
            System.out.println("#" + i + " -> " + types[i]);
            // types[i].completeType();
        }
    }

    public static final void dumpType(PrintStream out, Type type) {
        out.println("Type = " + type);
        out.println("Name = " + type.name);
        out.println("Namespace = " + type.namespace);
        out.println("FullName = " + type.fullName);
        out.println("Attributes = " + TypeAttributes.toString(type.Attributes));
        out.println("BaseType = " + type.BaseType());
        Type[] ifaces = type.getInterfaces();
        if (ifaces != null) {
            for (int i = 0; i < ifaces.length; i++)
                out.println("\timplements " + ifaces[i]);
        }
        out.println("Assembly = " + type.assembly());
        out.println("Module = " + type.Module);
        out.println("DeclaringType = " + type.declaringType);
        out.println("IsInterface = " + type.isInterface());
        out.println("IsAbstract = " + type.isAbstract());

        FieldInfo[] fields = type.getFields(BindingFlags.Instance
                | BindingFlags.Static | BindingFlags.NonPublic);
        out.println("\nFields (" + fields.length + "):");
        for (int i = 0; i < fields.length; i++) {
            out.println("\t" + fields[i]);
            out.println("\t\tDeclaringType = " + fields[i].declaringType);
//            out.println("\t\tReflectedType = " + fields[i].ReflectedType);
        }

        ConstructorInfo[] constrs = type.getConstructors();
        out.println("\nConstructors (" + constrs.length + "):");
        for (int i = 0; i < constrs.length; i++) {
            out.println("\t" + constrs[i]);
        }

        // MethodInfo[] methods = type.GetMethods(BindingFlags.Instance
        // | BindingFlags.Static
        // | BindingFlags.Public
        // | BindingFlags.NonPublic);
        MethodInfo[] methods = type.getMethods();
        out.println("\nMethods (" + methods.length + "):");
        for (int i = 0; i < methods.length; i++) {
            out.println("\t" + methods[i]);
            out.println("\t\tDeclaringType = " + methods[i].declaringType);
//            out.println("\t\tReflectedType = " + methods[i].ReflectedType);
        }
    }
}
