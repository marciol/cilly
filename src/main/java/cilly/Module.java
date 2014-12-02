/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly;

import java.util.Map;
import java.util.HashMap;

/**
 * Defines and represents a module. Get an instance of ModuleBuilder by calling
 * DefineDynamicModule A module is a portable executable file of type .dll or
 * .exe consisting of one or more classes and interfaces. There may be multiple
 * namespaces contained in a single module, and a namespace may span multiple
 * modules. One or more modules deployed as a unit compose an assembly.
 * 
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class Module extends CustomAttributeProvider {

    // ##########################################################################
    // public fields

    /** String representing the name of the module with the path removed. */
    public final String name;

    /** String representing the fully qualified name and path to this module. */
    public final String fullyQualifiedName;

    /** String representing the name of the module. */
    public String scopeName;

    /** The Assembly the Module belongs to. */
    public final Assembly assembly;

    // ##########################################################################
    // constructor

    protected Module(String name, String filename, String scopeName, Assembly assembly) {
        this.name = name;
        this.fullyQualifiedName = filename;
        this.scopeName = scopeName;
        this.assembly = assembly;
    }

    // ##########################################################################
    // public methods

    /** Returns the specified class, performing a case-sensitive search. */
    public Type getType(String name) {
        initTypes();
        return (Type) typesMap.get(name);
    }

    /**
     * @return all the classes defined within this module.
     */
    public Type[] getTypes() {
        initTypes();
        return types.clone();
    }

    /**
     * @return the global field with the specified name.
     */
    public FieldInfo getField(String name) {
        for (int i = 0; i < fields.length; i++)
            if (fields[i].name.equals(name))
                return fields[i];
        return null;
    }

    /**
     * @return an array of the global fields of the module
     */
    public FieldInfo[] getFields() {
        return fields.clone();
    }

    /**
     * @return - the global method with the specified name
     */
    public MethodInfo getMethod(String name) {
        for (int i = 0; i < methods.length; i++)
            if (methods[i].name.equals(name))
                return methods[i];
        return null;
    }

    /**
     * @return - an array of all the global methods defined in this modules.
     */
    public MethodInfo[] getMethods() {
        return methods.clone();
    }

    /**
     */
    @Override
    public String toString() {
        return name;
    }

    // ########################################################################
    // protected members

    // all the types defined in this module
    protected final Map typesMap = new HashMap<String, Type>();

    // all the types defined in this module
    protected Type[] types;

    // the global fields of the module
    protected FieldInfo[] fields = FieldInfo.EMPTY_ARRAY;

    // the global methods of the module
    protected MethodInfo[] methods = MethodInfo.EMPTY_ARRAY;

    protected Type addType(Type type) {
        addType(type.fullName, type);
        assembly.addType(type);
        return type;
    }

    protected Type addType(String name, Type type) {
        assert type != null;
        typesMap.put(name, type);
        return type;
    }

    private boolean initTypes = true;

    protected final void initTypes() {
        if (initTypes) {
            loadTypes();
            initTypes = false;
        }
    }

    protected void loadTypes() {
    }

    private boolean initGlobals = true;

    protected final void initGlobals() {
        if (initGlobals) {
            loadGlobals();
            initGlobals = false;
        }
    }

    protected void loadGlobals() {
    }

    // ##########################################################################

} // class Module
