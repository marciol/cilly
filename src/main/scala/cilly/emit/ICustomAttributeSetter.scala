/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package cilly.emit

import cilly.ConstructorInfo

/**
 * Declares the possibility to set a custom attribute for a member
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
trait ICustomAttributeSetter {
    def setCustomAttribute(constr: ConstructorInfo, value: Array[Byte]): Unit
}
