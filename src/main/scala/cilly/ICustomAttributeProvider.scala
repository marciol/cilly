/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

package cilly

/**
 * Provides custom attributes for reflection objects that support them.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
trait ICustomAttributeProvider {

    //##########################################################################
    // interface method definitions

    /** Returns an array of all of the custom attributes
     *  defined on this member, excluding named attributes,
     * 	or an empty array if there are no custom attributes.
     *
     *  @param inherit - When true, look up the hierarchy chain
     *                   for the inherited custom attribute.
     *  @return - An array of Objects representing custom attributes,
     *            or an empty array.
     */
    def getCustomAttributes(inherit: Boolean): Array[AnyRef]


    /** Returns an array of custom attributes defined on this member,
     *  identified by type, or an empty array
     *  if there are no custom attributes of that type.
     *
     *  @param attributeType - The type of the custom attributes.
     *  @param inherit - When true, look up the hierarchy chain
     *                   for the inherited custom attribute.
     *  @return - An array of Objects representing custom attributes,
     *            or an empty array.
     */
    def getCustomAttributes(attributeType: Type, inherit: Boolean): Array[AnyRef]


    /** Indicates whether one or more instance of attributeType
     *  is defined on this member
     *
     *  @param attributeType - The type of the custom attributes
     *  @param inherit - When true, look up the hierarchy chain
     *                   for the inherited custom attribute.
     *  @return - true if the attributeType is defined on this member;
     *            false otherwise.
     */
    def isDefined(attributeType: Type, inherit: Boolean): Boolean
}
