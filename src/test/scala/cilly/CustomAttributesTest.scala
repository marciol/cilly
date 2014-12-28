package cilly

object CustomAttributesTest {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("You must supply a filename!")
      System.exit(1)
    }
    val assem: Assembly = Assembly.loadFrom(args(0))
    Type.initMSCORLIB(assem)
    testCustomAttributes
  }

  def testCustomAttributes(): Unit = {
    val attrs: Array[AnyRef] = Type.getType("System.ObsoleteAttribute").getCustomAttributes(inherit = false)
    assert(attrs != null)

    attrs.foreach(attr => println(s"\t$attr") )
  }
}
