package cilly

/**
 * @author Miguel Garcia
 */
class GenericParamAndConstraints(val Number: Int, val Name: String, val Constraints: Array[Type], val isInvariant: Boolean, val isCovariant: Boolean, val isContravariant: Boolean, val isReferenceType: Boolean, val isValueType: Boolean, val hasDefaultConstructor: Boolean) {
  override def toString: String = {
    val nameString: String =
      if (Name == null || Name.isEmpty) "<NoName>"
      else Name
    s"$nameString <: $Constraints"
  }
}