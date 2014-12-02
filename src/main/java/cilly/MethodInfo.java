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
    public boolean hasPtrParamOrRetType() {
        if (returnType.isByRef() && !(returnType.getElementType().isValueType())) {
            /*
             * A method returning ByRef won't pass peverify, so I guess this is
             * dead code.
             */
            return true;
        }
        if (returnType.isPointer()) {
            return true;
        }
        return super.hasPtrParamOrRetType();
    }

    // ##########################################################################
    // public members

    @Override
    public final int memberType() {
        return MemberTypes.Method;
    }

    @Override
    public final boolean isConstructor() {
        return false;
    }

    /**
     * The return type of this method.
     */
    public final Type returnType;

    // ##########################################################################
    // protected members

    protected static final MethodInfo[] EMPTY_ARRAY = new MethodInfo[0];

    /**
     * Constructor Initializes a new instance of the MethodInfo class.
     */
    protected MethodInfo(String name, Type declType, int attrs, Type returnType, Type[] paramTypes) {
        super(name, declType, attrs, paramTypes);
        this.returnType = returnType;
        this.name = name;
        this.declaringType = declType;
    }

    protected MethodInfo(String name, Type declType, int attrs, Type returnType, ParameterInfo[] params) {
        super(name, declType, attrs, params);
        this.returnType = returnType;
        this.name = name;
        this.declaringType = declType;
    }
    
    public final String name;
    public final Type declaringType;

    @Override
    public String toString() {
        return MethodAttributes.toString(Attributes) + " " + returnType +
                " " + declaringType + "::" + name + params2String();
    }

    // ##########################################################################

} // class MethodInfo
