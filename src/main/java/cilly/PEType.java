/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly;

import cilly.PEFile.Sig;
import cilly.util.Table;
import cilly.util.Table.*;
import cilly.util.Signature;
import java.util.ArrayList;

/**
 * Represents a type from a .NET assembly
 * 
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final class PEType extends Type implements Signature {

    // ##########################################################################

    /** The PEFile that holds the description of the type. */
    final PEFile file;

    /** The number of the row in the TypeDef table defining the type. */
    final int definingRow;

    /** The row of the first method in the MethodDef table. */
    final int methodListBeg;

    /** The row of the last method in the MethodDef table + 1. */
    final int methodListEnd;

    /**
     * @param definingRow
     *            - the index in the TypeDef table where the type description
     *            is.
     */
    PEType(PEModule module, int attributes, String fullName, Type declType,
            int auxAttr, PEFile file, int definingRow) {
        super(module, attributes, fullName, null, null, declType, auxAttr);
        this.file = file;
        this.definingRow = definingRow;
        methodListBeg = file.typeDef(definingRow).MethodList;
        methodListEnd = definingRow < file.typeDef.rows ? file
                .typeDef(definingRow + 1).MethodList : file.methodDef.rows + 1;
    }

    // ##########################################################################
    // lazy type construction methods

    @Override
    protected void loadBaseType() {
        TypeDef type = file.typeDef(definingRow);
        baseType = type.Extends == 0 ? null : ((PEModule) Module)
                .getTypeDefOrRef(type.Extends);
    }

    @Override
    protected void loadFields() {
        // the list of the declared fields starts from the
        // FieldList index in the TypeDef table up to the smaller of the:
        // - the last row of the FieldDef table
        // - the start of the next list of fields determined by the
        // FieldList index of the next row in the TypeDef table
        final ArrayList<FieldInfo> fields = new ArrayList<FieldInfo>();
        int fieldListBeg = file.typeDef(definingRow).FieldList;
        int fieldListEnd = file.FieldDef.rows + 1;
        if (definingRow < file.typeDef.rows)
            fieldListEnd = file.typeDef(definingRow + 1).FieldList;

        for (int row = fieldListBeg; row < fieldListEnd; row++) {
            int frow = file.FieldTrans.rows == 0 ? row
                    : file.FieldTrans(row).Field;
            int attrs = file.FieldDef(frow).Flags;
            String name = file.FieldDef.getName();
            // System.out.println("\t-->Loading field: " + name);
            Sig sig = file.FieldDef.getSignature();
            cilly.util.PECustomMod pecmod = sig.decodeFieldType();
            Object val = null;
            Table.Constant consts = file.Constant;
            for (int i = 1; i <= consts.rows; i++) {
                consts.readRow(i);
                int tableId = Table.getTableId(Table._HasConstant,
                        consts.Parent);
                int refRow = consts.Parent >> Table.NoBits[Table._HasConstant];
                if (tableId == Table.FieldDef.ID && refRow == frow)
                    val = consts.getValue();
            }
            FieldInfo field = new PEFieldInfo(row, name, attrs, pecmod, val);
            if (field.name.equals("value__") && field.isSpecialName()) {
                assert underlyingType == null : underlyingType.toString();
                underlyingType = field.fieldType;
            }
            fields.add(field);
        }
        this.fields = fields.toArray(FieldInfo.EMPTY_ARRAY);
        fields.clear();
    }

    protected MethodBase[] methoddefs;

    protected MethodInfo getMethod(int n) {
        return (MethodInfo) methoddefs[n - methodListBeg];
    }

    @Override
    protected void loadMethods() {
        methoddefs = new MethodBase[methodListEnd - methodListBeg];

        final ArrayList<MethodInfo> methods = new ArrayList<MethodInfo>();
        final ArrayList<ConstructorInfo> constrs = new ArrayList<ConstructorInfo>();
        PEModule pemodule = (PEModule) Module;
        for (int row = methodListBeg; row < methodListEnd; row++) {
            int mrow = file.MethodTrans.rows == 0 ? row
                    : file.MethodTrans(row).Method;
            int attrs = file.methodDef(mrow).Flags;
            String name = file.methodDef.getName();
            Sig sig = file.methodDef.getSignature();
            /*
             * we're about to parse a MethodDefSig, defined in Sec. 23.2.1 of
             * Partition II ()
             */

            int callConv = sig.readByte();
            // TODO decode HASTHIS from high byte of calling convention
            // TODO decode EXPLICITTHIS from high byte of calling convention
            // TODO handle VARARG calling convention (not CLS but may show up )
            if ((callConv & 0x1F) == Signature.GENERIC) {
                int genParamCount = sig.decodeInt();
                /*
                 * genParamCount is ignored because the method's type params
                 * will be obtained below (see:
                 * file.GenericParam.getMVarIdxes(row) )
                 */
            }
            int paramCount = sig.decodeInt();
            Type retType = sig.decodeRetType();
            Type[] paramType = new Type[paramCount];
            for (int i = 0; i < paramCount; i++)
                paramType[i] = sig.decodeParamType();

            ParameterInfo[] params = new ParameterInfo[paramCount];
            int paramListBeg = file.methodDef.ParamList;
            int paramListEnd = paramListBeg + paramCount;
            if (paramListEnd > file.ParamDef.rows) {
                /*
                 * don't try to read param names past ParamDef's row count Some
                 * assembly-writers don't bother to give names for all params.
                 */
                paramListEnd = file.ParamDef.rows + 1;
            }
            for (int i = paramListBeg; i < paramListEnd; i++) {
                int pattr = file.ParamDef(i).Flags;
                String paramName = file.ParamDef.getName();
                int seq = file.ParamDef.Sequence;
                if (seq == 0) {
                    // System.out.println("Retval attributes 0x" +
                    // PEFile.short2hex(pattr));
                } else {
                    params[seq - 1] = new ParameterInfo(paramName,
                            paramType[seq - 1], pattr, seq - 1);
                }
            }
            for (int i = 0; i < params.length; i++) {
                if (params[i] == null)
                    params[i] = new ParameterInfo(null, paramType[i], 0, 0);
            }
            if ((attrs & MethodAttributes.SpecialName) != 0
                    && (attrs & MethodAttributes.RTSpecialName) != 0
                    && (name.equals(ConstructorInfo.CTOR()) || name
                            .equals(ConstructorInfo.CCTOR()))) {
                ConstructorInfo constr = new PEConstructorInfo(row, attrs, params);
                constrs.add(constr);
                methoddefs[row - methodListBeg] = constr;
            } else {
                MethodInfo method = new PEMethodInfo(row, name, attrs, retType, params);
                int[] mvarIdxes = file.GenericParam.getMVarIdxes(row);
                // if(mvarIdxes.length > 0) { System.out.println("Method: " +
                // method); }
                for (int i = 0; i < mvarIdxes.length; i++) {
                    GenericParamAndConstraints mvarAndConstraints = pemodule
                            .getTypeConstraints(mvarIdxes[i]);
                    // add mvarAndConstraints as i-th MVar in method
                    method.addMVar(mvarAndConstraints);
                }
                methods.add(method);
                methoddefs[row - methodListBeg] = method;
            }
        }

        this.constructors = constrs
                .toArray(ConstructorInfo.EMPTY_ARRAY());
        this.methods = methods.toArray(MethodInfo.EMPTY_ARRAY);
        constrs.clear();
        methods.clear();
    }

    @Override
    protected void loadProperties() {
        final PropertyMap pmap = file.PropertyMap;
        if (pmap == null) {
            properties = PropertyInfo.EMPTY_ARRAY;
            return;
        }

        final PropertyDef pdef = file.PropertyDef;
        int propListBeg = -1;
        int propListEnd = pdef.rows + 1;
        for (int i = 1; i <= pmap.rows; i++) {
            pmap.readRow(i);
            if (pmap.Parent == this.definingRow) {
                propListBeg = pmap.PropertyList;
                if (i < pmap.rows) {
                    pmap.readRow(i + 1);
                    propListEnd = pmap.PropertyList;
                }
                break;
            }
        }
        if (propListBeg < 0) {
            properties = PropertyInfo.EMPTY_ARRAY;
            return;
        }

        final ArrayList<PropertyInfo> properties = new ArrayList<PropertyInfo>();
        for (int i = propListBeg; i < propListEnd; i++) {
            pdef.readRow(i);
            Sig sig = pdef.getSignature();
            int b = sig.readByte();
            b &= ~HASTHIS;
            int paramCount = sig.readByte();
            assert b == PROPERTY;
            Type propType = sig.decodeType();
            int index = Table.encodeIndex(i, Table._HasSemantics,
                    Table.PropertyDef.ID);
            MethodSemantics msem = file.MethodSemantics;
            MethodInfo getter = null, setter = null;
            for (int j = 1; j <= msem.rows; j++) {
                msem.readRow(j);
                if (msem.Association != index)
                    continue;
                if (msem.isGetter())
                    getter = getMethod(msem.Method);
                else if (msem.isSetter())
                    setter = getMethod(msem.Method);
                else
                    System.err.println("PEType.loadProperties(): !?!");
            }
            properties.add(new PEPropertyInfo(i, pdef.getName(),
                    (short) pdef.Flags, propType, getter, setter));
        }
        this.properties = properties
                .toArray(PropertyInfo.EMPTY_ARRAY);
    }

    @Override
    protected void loadEvents() {
        EventMap emap = file.EventMap;
        if (emap == null) {
            this.events = EventInfo.EMPTY_ARRAY();
            return;
        }

        final EventDef edef = file.EventDef;
        int eventListBeg = -1;
        int eventListEnd = edef.rows + 1;
        for (int i = 1; i <= emap.rows; i++) {
            emap.readRow(i);
            if (emap.Parent == this.definingRow) {
                eventListBeg = emap.EventList;
                if (i < emap.rows) {
                    emap.readRow(i + 1);
                    eventListEnd = emap.EventList;
                }
                break;
            }
        }
        if (eventListBeg < 0) {
            this.events = EventInfo.EMPTY_ARRAY();
            return;
        }

        final ArrayList<PEEventInfo> events = new ArrayList<PEEventInfo>();
        final MethodSemantics msem = file.MethodSemantics;
        for (int i = eventListBeg; i < eventListEnd; i++) {
            edef.readRow(i);
            final Type handler = ((PEModule) Module)
                    .getTypeDefOrRef(edef.EventType);
            int index = Table.encodeIndex(i, Table._HasSemantics,
                    Table.EventDef.ID);
            MethodInfo add = null, remove = null;
            for (int j = 1; j <= msem.rows; j++) {
                msem.readRow(j);
                if (msem.Association != index)
                    continue;
                if (msem.isAddOn())
                    add = getMethod(msem.Method);
                else if (msem.isRemoveOn())
                    remove = getMethod(msem.Method);
                else {
                }
            }
            events.add(new PEEventInfo(i, edef.getName(),
                    (short) edef.EventFlags, handler, add, remove));
        }
        this.events = events.toArray(EventInfo.EMPTY_ARRAY());
    }

    @Override
    protected void loadNestedTypes() {
        final ArrayList<Type> nested = new ArrayList<Type>();
        for (int i = 1; i <= file.NestedClass.rows; i++) {
            file.NestedClass.readRow(i);
            if (file.NestedClass.enclosingClass == this.definingRow)
                nested.add(((PEModule) Module)
                        .getTypeDef(file.NestedClass.nestedClass));
        }
        this.nestedTypes = nested.toArray(Type.EmptyTypes);
    }

    @Override
    protected void loadInterfaces() {
        // get the interfaces implemented by this class
        interfaces = Type.EmptyTypes;
        int index = file.InterfaceImpl.findType(definingRow);
        if (index > 0) {
            ArrayList<Type> ifaces = new ArrayList<Type>();
            for (int i = index; i <= file.InterfaceImpl.rows; i++) {
                file.InterfaceImpl.readRow(i);
                if (file.InterfaceImpl.Class != definingRow)
                    break;
                ifaces.add(((PEModule) Module)
                        .getTypeDefOrRef(file.InterfaceImpl.Interface));
            }
            interfaces = ifaces.toArray(new Type[ifaces.size()]);
        }
    }

    @Override
    protected void loadCustomAttributes(Type attributeType) {
        initAttributes(this, definingRow, Table.TypeDef.ID, attributeType);
    }

    private void initAttributes(CustomAttributeProvider cap, int definingRow,
            int sourceTableId, Type attributeType) {
        ((PEModule) this.Module).initAttributes(cap, definingRow,
                sourceTableId, attributeType);
    }

    // ##########################################################################

    private class PEFieldInfo extends FieldInfo {
        private final int definingRow;

        public PEFieldInfo(int definingRow, String name, int attrs,
                cilly.util.PECustomMod pecmod, Object value) {
            super(name, PEType.this, attrs, pecmod, value);
            this.definingRow = definingRow;
        }

        @Override
        protected void loadCustomAttributes(Type attributeType) {
            PEType.this.initAttributes(this, definingRow, Table.FieldDef.ID,
                    attributeType);
        }
    }

    private class PEMethodInfo extends MethodInfo {
        private final int definingRow;

        public PEMethodInfo(int row, String name, int attrs, Type retType,
                ParameterInfo[] params) {
            super(name, PEType.this, attrs, retType, params);
            this.definingRow = row;
        }

        @Override
        protected void loadCustomAttributes(Type attributeType) {
            PEType.this.initAttributes(this, definingRow, Table.MethodDef.ID,
                    attributeType);
        }
    }

    private class PEConstructorInfo extends ConstructorInfo {
        private final int definingRow;

        public PEConstructorInfo(int row, int attrs, ParameterInfo[] params) {
            super(PEType.this, attrs, params);
            this.definingRow = row;
        }

        @Override
        protected void loadCustomAttributes(Type attributeType) {
            PEType.this.initAttributes(this, definingRow, Table.MethodDef.ID,
                    attributeType);
        }
    }

    private class PEPropertyInfo extends PropertyInfo {
        private final int definingRow;

        public PEPropertyInfo(int row, String name, short attrs, Type propType,
                MethodInfo getter, MethodInfo setter) {
            super(name, PEType.this, attrs, propType, getter, setter);
            this.definingRow = row;
        }

        @Override
        protected void loadCustomAttributes(Type attributeType) {
            PEType.this.initAttributes(this, definingRow, Table.PropertyDef.ID,
                    attributeType);
        }
    }

    private class PEEventInfo extends EventInfo {
        private final int definingRow;

        public PEEventInfo(int row, String name, short attrs, Type handler,
                MethodInfo add, MethodInfo remove) {
            super(name, PEType.this, attrs, handler, add, remove);
            this.definingRow = row;
        }

        @Override
        protected void loadCustomAttributes(Type attributeType) {
            PEType.this.initAttributes(this, definingRow, Table.EventDef.ID,
                    attributeType);
        }
    }

    // ##########################################################################

} // class PEType
