/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */


package cilly;


/**
 * Discovers the attributes of an event
 * and provides access to event metadata.
 *
 * attributes: Attributes associated with the event.
 * handlerType: The Type object for the underlying event-handler delegate
 *              associated with this event.
 * 
 * @author Nikolay Mihaylov
 * @version 1.0
 */
class EventInfo(override val name: String, override val declaringType: Type, val attributes: Short, val handlerType: Type, val addMethod: MethodInfo, val removeMethod: MethodInfo)
    extends MemberInfo(name, declaringType) {

    final def memberType = MemberTypes.Event
 
    override def toString = s"$handlerType $name"
}

final case object EventInfo {
  final val EMPTY_ARRAY: Array[EventInfo] = new Array[EventInfo](0)
}
