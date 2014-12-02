/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly;

/**
 * Discovers the attributes of a field and provides access to field metadata.
 * 
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public class FieldInfo extends MemberInfo implements HasCustomModifiers {

    // ##########################################################################
    // public interface

    @Override
    public final int memberType() {
        return MemberTypes.Field;
    }

    /** Attributes associated with this field. */
    public final short attributes;

    /** Type of the field represented by this FieldInfo object. */
    public final Type fieldType;

    /** can be null */
    public final CustomModifier[] cmods;

    protected final Object value;

    public final boolean isStatic() {
        return (attributes & FieldAttributes.Static) != 0;
    }

    public final boolean isInitOnly() {
        return (attributes & FieldAttributes.InitOnly) != 0;
    }

    public final boolean isLiteral() {
        return (attributes & FieldAttributes.Literal) != 0;

    }

    public final boolean isPublic() {
        return (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.Public;
    }

    public final boolean isPrivate() {
        return (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.Private;
    }

    public final boolean isFamily() {
        return (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.Family;
    }

    public final boolean isAssembly() {
        return (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.Assembly;
    }

    public final boolean isFamilyOrAssembly() {
        return (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.FamORAssem;
    }

    public final boolean isFamilyAndAssembly() {
        return (attributes & FieldAttributes.FieldAccessMask) == FieldAttributes.FamANDAssem;
    }

    public final boolean isSpecialName() {
        return (attributes & FieldAttributes.SpecialName) != 0;
    }

    public final boolean isPinvokeImpl() {
        return (attributes & FieldAttributes.PinvokeImpl) != 0;
    }

    public final boolean isNotSerialized() {
        return (attributes & FieldAttributes.NotSerialized) != 0;
    }

    private boolean knownVolatile = false;
    private boolean cachedVolatile = false;

    public final boolean isVolatile() {
        if (knownVolatile)
            return cachedVolatile;
        knownVolatile = true;
        if (cmods == null) {
            cachedVolatile = false;
            return cachedVolatile;
        }
        for (int idx = 0; idx < cmods.length; idx++) {
            if (cmods[idx].marker == CustomModifier.VolatileMarker()) {
                cachedVolatile = true;
                return cachedVolatile;
            }
        }
        cachedVolatile = false;
        return cachedVolatile;
    }

    @Override
    public final Type[] getOptionalCustomModifiers() {
        return CustomModifier.helperCustomMods(false, cmods);
    }

    @Override
    public final Type[] getRequiredCustomModifiers() {
        return CustomModifier.helperCustomMods(true, cmods);
    }

    @Override
    public String toString() {
        return FieldAttributes.toString(attributes) + " " + fieldType + " "
                + declaringType.fullName + "::" + name;
    }

    // ##########################################################################

    protected static final FieldInfo[] EMPTY_ARRAY = new FieldInfo[0];

    /** Initializes a new instance of the FieldInfo class. */
    protected FieldInfo(String name, Type declType, int attrs,
            cilly.util.PECustomMod fieldTypeWithMods, Object value) {
        super(name, declType);
        fieldType = fieldTypeWithMods.marked();
        cmods = fieldTypeWithMods.cmods();
        attributes = (short) attrs;
        this.value = value;
        this.name = name;
        this.declaringType = declType;
    }
    
    public final String name;
    public final Type declaringType;

    /**
     */
    public Object getValue() {
        return value;
    }

    // ##########################################################################

} // class FieldInfo
