/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly

/**
 * Discovers the attributes of a parameter and provides access to
 * parameter metadata.
 *
 * Attributes of the parameter.
 * Name of the parameter.
 * Type of the parameter.
 * Position of the parameter in the parameter list.
 * 
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class ParameterInfo protected (val name: String, val parameterType: Type, val attributes: Short, val position: Int) extends CustomAttributeProvider {

  protected def this(name: String, parameterType: Type, attributes: Int, position: Int) {
    this(name, parameterType, attributes.toShort, position)
  }
  
    /** Is this an input parameter? */
    def isIn: Boolean = (attributes & ParameterAttributes.In) != 0

    /** Is this an output parameter? */
    def isOut: Boolean = (attributes & ParameterAttributes.Out) != 0

    /** Is this an Lcid? */
    def isLcid: Boolean = (attributes & ParameterAttributes.Lcid) != 0

    /** Is this a return value? */
    def isRetval: Boolean = (attributes & ParameterAttributes.Retval) != 0

    /** Is this an optional parameter? */
    def isOptional: Boolean = (attributes & ParameterAttributes.Optional) != 0

    override def toString = s"${ParameterAttributes.toString(attributes)}$parameterType $name"
}
