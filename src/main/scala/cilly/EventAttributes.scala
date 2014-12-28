/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */
package cilly

/**
 * Specifies flags that describe the attributes of a an event.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object EventAttributes {
  /** Specifies that the event has no attributes. */
  val None: Short = 0x000
  /** Specifies a reserved flag for CLR use only. */
  val ReservedMask: Short = 0x0400
  /** Specifies that the event is special in a way described by the name. */
  val SpecialName: Short = 0x0200
  /** Specifies the the CLR should check name encoding. */
  val RTSpecialName: Short = 0x0400
}