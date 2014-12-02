/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;

/**
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class CustomAttributeProvider implements ICustomAttributeProvider {

    // ##########################################################################

    protected List<Attribute> custAttrs;
    private static final Object[] EMPTY = new Object[0];

    // TODO: take inherit into account
    @Override
    public Object[] getCustomAttributes(boolean inherit) {
        initAttributes(null);
        return custAttrs.size() == 0 ? EMPTY : custAttrs
                .toArray(new Attribute[custAttrs.size()]);
    }

    // TODO: take inherit into account
    @Override
    public Object[] getCustomAttributes(Type attributeType, boolean inherit) {
        initAttributes(attributeType);
        List<Attribute> tAttrs = null;
        if (constrType == attributeType)
            tAttrs = custAttrs;
        else {
            tAttrs = new LinkedList<Attribute>();
            for (Iterator<Attribute> attrs = custAttrs.iterator(); attrs
                    .hasNext();) {
                Attribute a = attrs.next();
                if (a.getType() == attributeType)
                    tAttrs.add(a);
            }
        }
        return tAttrs.size() == 0 ? EMPTY : tAttrs.toArray(new Attribute[tAttrs
                .size()]);
    }

    // TODO: take inherit into account
    @Override
    public boolean isDefined(Type attributeType, boolean inherit) {
        initAttributes(attributeType);
        if (constrType == attributeType)
            return custAttrs.size() > 0;
        Iterator<Attribute> attrs = custAttrs.iterator();
        while (attrs.hasNext()) {
            if (attrs.next().getType() == attributeType)
                return true;
        }
        return false;
        // return inherit && (DeclaringClass.BaseType != null)
        // && DeclaringClass.BaseType.IsDefined(inherit);
    }

    protected void addCustomAttribute(ConstructorInfo constr, byte[] value) {
        Attribute attr = new Attribute(constr, value);
        assert constrType == null || constrType == attr.getType();
        if (custAttrs == null)
            custAttrs = new LinkedList<Attribute>();
        custAttrs.add(attr);
    }

    private void initAttributes(Type atype) {
        if (custAttrs != null && (constrType == null || constrType == atype))
            return;
        custAttrs = new LinkedList<Attribute>();
        constrType = atype;
        loadCustomAttributes(atype);
    }

    protected void loadCustomAttributes(Type atype) {
    }

    private Type constrType;
}
