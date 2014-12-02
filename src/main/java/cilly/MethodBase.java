/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly;

import java.util.Iterator;

/**
 * The common superclass of MemberInfo and ConstructorInfo
 * 
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class MethodBase extends MemberInfo {

    // ##########################################################################
    // public interface

    private final java.util.List<GenericParamAndConstraints> mVars = new java.util.LinkedList<GenericParamAndConstraints>();
    private GenericParamAndConstraints[] sortedMVars = null;

    public void addMVar(GenericParamAndConstraints tvarAndConstraints) {
        sortedMVars = null;
        mVars.add(tvarAndConstraints);
    }

    public GenericParamAndConstraints[] getSortedMVars() {
        if (sortedMVars == null) {
            sortedMVars = new GenericParamAndConstraints[mVars.size()];
            for (int i = 0; i < sortedMVars.length; i++) {
                Iterator<GenericParamAndConstraints> iter = mVars.iterator();
                while (iter.hasNext()) {
                    GenericParamAndConstraints tvC = iter.next();
                    if (tvC.Number == i) {
                        sortedMVars[i] = tvC;
                    }
                }
            }
        }
        return sortedMVars;
    }

    public final boolean isGeneric() {
        return mVars.size() > 0;
    }

    /** The attributes associated with this method/constructor. */
    public final short Attributes;

    /***/
    public final short callingConvention;

    public abstract boolean isConstructor();

    public final boolean isAbstract() {
        return (Attributes & MethodAttributes.Abstract) != 0;
    }

    public final boolean isFinal() {
        return (Attributes & MethodAttributes.Final) != 0;
    }

    public final boolean isVirtual() {
        return (Attributes & MethodAttributes.Virtual) != 0;
    }

    public final boolean isInstance() {
        return !isStatic() && !isVirtual();
    }

    public final boolean isStatic() {
        return (Attributes & MethodAttributes.Static) != 0;
    }

    public final boolean isHideBySig() {
        return (Attributes & MethodAttributes.HideBySig) != 0;
    }

    public final boolean isSpecialName() {
        return (Attributes & MethodAttributes.SpecialName) != 0;
    }

    public final boolean isPublic() {
        return (Attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.Public;
    }

    public final boolean isPrivate() {
        return (Attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.Private;
    }

    public final boolean isFamily() {
        return (Attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.Family;
    }

    public final boolean isAssembly() {
        return (Attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.Assembly;
    }

    public final boolean isFamilyOrAssembly() {
        return (Attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.FamORAssem;
    }

    public final boolean isFamilyAndAssembly() {
        return (Attributes & MethodAttributes.MemberAccessMask) == MethodAttributes.FamANDAssem;
    }

    public boolean hasPtrParamOrRetType() {
        // the override in MethodInfo checks the return type
        ParameterInfo[] ps = getParameters();
        for (int i = 0; i < ps.length; i++) {
            Type pT = ps[i].parameterType();
            if (pT.isPointer()) {
                // Type.mkPtr creates a msil.Type for a pointer type
                return true;
            }
            if (pT.isByRef() && !pT.getElementType().canBeTakenAddressOf()) {
                /*
                 * TODO Cases where GenMSIL (so far) con't emit good bytecode:
                 * the type being taken address of IsArray(), IsGeneric(), or
                 * IsTMVarUsage. For example, System.Enum declares public static
                 * bool TryParse<TEnum>(string value, out TEnum result) where
                 * TEnum : struct, new();
                 */
                return true;
            }
        }
        return false;
    }

    /** Returns the parameters of the method/constructor. */
    public ParameterInfo[] getParameters() {
        return params.clone();
    }

    public int getMethodImplementationFlags() {
        return implAttributes;
    }

    // ##########################################################################

    /** Method parameters. */
    protected ParameterInfo[] params;

    protected short implAttributes;
    
    protected static ParameterInfo[] convertParamTypesToParameterInfos(Type[] paramTypes) {
        assert paramTypes != null;
        ParameterInfo[] params = new ParameterInfo[paramTypes.length];
        for (int i = 0; i < params.length; i++)
            params[i] = new ParameterInfo(null, paramTypes[i], 0, i);
        return params;
    }

    protected MethodBase(String name, Type declType, int attrs, Type[] paramTypes) {
        this(name, declType, attrs);
        params = convertParamTypesToParameterInfos(paramTypes);
    }

    protected MethodBase(String name, Type declType, int attrs, ParameterInfo[] params) {
        this(name, declType, attrs);
        this.params = params;
    }

    /**
     */
    private MethodBase(String name, Type declType, int attrs) {
        super(name, declType);
        this.name = name;
        this.declaringType = declType;

        Attributes = (short) attrs;

        if (isConstructor()) {
            attrs |= MethodAttributes.SpecialName;
            attrs |= MethodAttributes.RTSpecialName;
        }

        callingConvention = (short) (CallingConventions.Standard() | (isStatic() ? (short) 0 : CallingConventions.HasThis()));
    }

    public final String name;
    public final Type declaringType;

    // ##########################################################################
    // internal methods

    protected String params2String() {
        StringBuffer s = new StringBuffer("(");
        for (int i = 0; i < params.length; i++) {
            if (i > 0)
                s.append(", ");
            s.append(params[i].parameterType());
        }
        s.append(")");
        return s.toString();
    }

    // ##########################################################################

} // class MethodBase
