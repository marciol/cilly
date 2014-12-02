/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly;

/**
 * Discovers the attributes of a method and provides access to method metadata.
 * 
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class MethodInfo extends MethodBase {

    @Override
    public boolean HasPtrParamOrRetType() {
        if (ReturnType.isByRef() && !(ReturnType.getElementType().isValueType())) {
            /*
             * A method returning ByRef won't pass peverify, so I guess this is
             * dead code.
             */
            return true;
        }
        if (ReturnType.isPointer()) {
            return true;
        }
        return super.HasPtrParamOrRetType();
    }

    // ##########################################################################
    // public members

    @Override
    public final int memberType() {
        return MemberTypes.Method;
    }

    @Override
    public final boolean IsConstructor() {
        return false;
    }

    /**
     * The return type of this method.
     */
    public final Type ReturnType;

    // ##########################################################################
    // protected members

    protected static final MethodInfo[] EMPTY_ARRAY = new MethodInfo[0];

    /**
     * Constructor Initializes a new instance of the MethodInfo class.
     */
    protected MethodInfo(String name, Type declType, int attrs, Type returnType, Type[] paramTypes) {
        super(name, declType, attrs, paramTypes);
        this.ReturnType = returnType;
        this.name = name;
        this.declaringType = declType;
    }

    protected MethodInfo(String name, Type declType, int attrs, Type returnType, ParameterInfo[] params) {
        super(name, declType, attrs, params);
        this.ReturnType = returnType;
        this.name = name;
        this.declaringType = declType;
    }
    
    public final String name;
    public final Type declaringType;

    @Override
    public String toString() {
        return MethodAttributes.toString(Attributes) + " " + ReturnType +
                " " + declaringType + "::" + name + params2String();
    }

    // ##########################################################################

} // class MethodInfo
