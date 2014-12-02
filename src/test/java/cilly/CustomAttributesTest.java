package cilly;

public class CustomAttributesTest {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("You must supply a filename!");
            System.exit(1);
        }

        Assembly assem = Assembly.loadFrom(args[0]);
        Type.initMSCORLIB(assem);

        testCustomAttributes();
    }

    public static void testCustomAttributes() {
        Object[] attrs = Type.getType("System.ObsoleteAttribute").getCustomAttributes(false);
        assert attrs != null;
        for (int i = 0; i < attrs.length; i++) {
            System.out.println("\t" + attrs[i]);
        }
    }

}
