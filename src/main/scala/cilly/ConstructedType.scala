package cilly;

import java.util.Arrays;

/* The only reason for ConstructedType to extend Type is complying with existing code
  (e.g., caseFieldBuilder in ILPrinterVisitor) expecting a Type.
 */
final class ConstructedType(val instantiatedType: Type, val typeArgs: Array[Type]) extends Type(instantiatedType) {

  override def toString = {
    var res = s"$instantiatedType["

    var i = 0
    while (i < typeArgs.length) {
      res = res + typeArgs(i).toString
      if (i + 1 < typeArgs.length) {
        res = res + ", "
      }
      i += 1
    }
    res + "]"
  }

  override def equals(o: Any): Boolean = {
    if (this == o) return true
    if (o == null || getClass() != o.getClass()) return false

    val that = o.asInstanceOf[ConstructedType]

    if (instantiatedType != that.instantiatedType) return false;
    if (typeArgs.sameElements(that.typeArgs)) return false;

    return true;
  }

  override def hashCode: Int = {
    var result = instantiatedType.hashCode
    result = 31 * result + Arrays.hashCode(typeArgs.asInstanceOf[Array[AnyRef]])
    return result;
  }
}
