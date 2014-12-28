/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */

package cilly.emit

import cilly._
import cilly.util.Table
import java.util.Stack
import java.io.IOException
import ILGenerator._

/**
 * Generates Common Intermediate Language (CIL) instructions.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
final class ILGenerator(val owner: MethodBase) extends Visitable {

  //##########################################################################
  // public interface

  /**
   * Puts the specified instruction onto the stream of instructions.
   */
  def emit(opcode: OpCode): Unit = {
    // switch opcode
    if (opcode == OpCode.Ret) {
      emit(opcode, null, 0)
    } else {
      emit(opcode, (null: Object))
    }
  }

  /**
   * Puts the specified instruction and character argument onto
   * the Common Intermediate Language (CIL) stream of instructions.
   */
  def emit(opcode: OpCode, arg: Char): Unit = {
    emit(opcode, new Character(arg))
  }

  /**
   * Puts the specified instruction and metadata token for the
   * specified constructor onto the Common Intermediate Language
   * (CIL) stream of instructions.
   */
  def emit(opcode: OpCode, arg: ConstructorInfo): Unit = {
    assert(arg != null)
    // newobj
    // pop size is the number of parameters
    emit(opcode, arg, OpCode.PUSH_size(opcode.CEE_push) -
      arg.getParameters.length)
  }

  /**
   * Puts the specified instruction onto the Common Intermediate Language (CIL)
   * stream followed by the index of the given local variable.
   */
  def emit(opcode: OpCode, arg: LocalBuilder): Unit = {
    assert(arg != null)
    // ldarg    | ldarg.s  | ldarga
    // ldarga.s  | ldloc    | ldloc.s  | ldloca
    // ldloca.s  | starg    | starg.s  | stloc
    // stloc.s

    // <instr_var> <localname>
    emit(opcode, arg)
  }

  /**
   * Puts the specified instruction and numerical argument onto
   * the Common Intermediate Language (CIL) stream of instructions.
   */
  def emit(opcode: OpCode, arg: Double): Unit = {
    // ldc.r4 | ldc.r8
    emit(opcode, new java.lang.Double(arg))
  }

  /**
   * Puts the specified instruction and metadata token for the
   * specified field onto the Common Intermediate Language (CIL)
   * stream of instructions.
   */
  def emit(opcode: OpCode, arg: FieldInfo): Unit = {
    assert(arg != null)
    // ldfld | ldflda | ldsfld | ldsflda | stfld | stsfld
    emit(opcode, arg)
  }

  /**
   * Puts the specified instruction and numerical argument onto
   * the Common Intermediate Language (CIL) stream of instructions.
   */
  def emit(opcode: OpCode, arg: Short): Unit = {
    emit(opcode, new java.lang.Short(arg))
  }

  /**
   * Puts the specified instruction and numerical argument onto
   * the Common Intermediate Language (CIL) stream of instructions.
   */
  def emit(opcode: OpCode, arg: Int): Unit = {
    // ldc.i4 | ldc.i4.s | unaligned
    emit(opcode, new java.lang.Integer(arg))
  }

  /**
   * Puts the specified instruction and numerical argument onto
   * the Common Intermediate Language (CIL) stream of instructions.
   */
  def emit(opcode: OpCode, arg: Long): Unit = {
    // ldc.i8
    emit(opcode, new java.lang.Long(arg))
  }

  /**
   * Puts the specified instruction onto the Microsoft intermediate
   * language (CIL) stream and leaves space to include a label when
   * fixes are done.
   */
  def emit(opcode: OpCode, label: Label): Unit = {
    assert(label != null)
    // beq    | beq.s    | bge    | bge.s    |
    // bge.un    | bge.un.s   | bgt    | bgt.s    | bgt.un | bgt.un.s |
    // ble       | ble.s      | ble.un | ble.un.s | blt    | blt.s    |
    // blt.un    | blt.un.s   | bne.un | bne.un.s | br     | br.s     |
    // brfalse   | brfalse.s  | brtrue | brtrue.s | leave  | leave.s

    emit(opcode, label)
    // is the label initialized ? if true backward jump else forward jump
    if (label.isInitialized) {
      // 	    if (arg.stacksize != lastLabel.stacksize) {
      // 		System.err.println("ILGenerator.Emit: Stack depth differs depending on path:");
      // 		System.err.println("\tmethod = " + owner);
      // 		System.err.println("\tPC = 0x" + Table.short2hex(lastLabel.address));
      // 	    }
      //assert arg.stacksize == lastLabel.stacksize;
    } else {
      label.setStacksize(lastLabel.getStacksize)
    }
  }

  /**
   * Puts the specified instruction onto the Microsoft intermediate
   * language (CIL) stream and leaves space to include a label when
   * fixes are done.
   */
  def emit(opcode: OpCode, arg: Array[Label]): Unit = {
    assert(arg != null)
    // switch

    // <instr> ::= <instr_switch> ( <labels> )
    // Examples:
    // switch (0x3, -14, Label1)
    // switch (5, Label2)
    emit(opcode, arg, arg.length)
  }

  /**
   * Puts the specified instruction onto the Microsoft intermediate
   * language (CIL) stream followed by the metadata token for the
   * given method.
   */
  def emit(opcode: OpCode, arg: MethodInfo): Unit = {
    assert(arg != null)
    // call  | callvirt | jmp | ldftn | ldvirtftn
    // pop size is the number of parameters
    // pop 1 more if method is not static !
    // push size is either 0 (void Method) either 1
    assert(arg.returnType != null, "No ReturnType: " + arg.declaringType + "::" + arg.name)

    val popush: Int = if (opcode == OpCode.Ldftn ||
      opcode == OpCode.Ldvirtftn ||
      opcode == OpCode.Jmp) {
      OpCode.PUSH_size(opcode.CEE_push) - OpCode.POP_size(opcode.CEE_pop)
    } else if (opcode == OpCode.Calli || opcode == OpCode.Callvirt) {
      (if (arg.returnType == Common.SystemVoid) 0 else 1) - arg.getParameters.length - 1
    } else {
      (if (arg.returnType == Common.SystemVoid) 0 else 1) - arg.getParameters.length
    }
    emit(opcode, arg, popush)
  }

  /**
   * Puts the specified instruction and numerical argument onto
   * the Common Intermediate Language (CIL) stream of instructions.
   */
  def emit(opcode: OpCode, arg: Float): Unit = {
    emit(opcode, new java.lang.Float(arg))
  }

  /**
   * Puts the specified instruction onto the Microsoft intermediate
   * language (CIL) stream followed by the metadata token for the
   * given string.
   */
  def emit(opcode: OpCode, arg: String): Unit = {
    assert(arg != null)
    // ldstr
    emit(opcode, arg)
  }

  /**
   * Puts the specified instruction onto the Microsoft intermediate
   * language (CIL) stream followed by the metadata token for the
   * given type.
   */
  def emit(opcode: OpCode, arg: Type): Unit = {
    assert(arg != null)
    // box     | castclass | cpobj    | initobj | isinst    |
    // ldelema | ldobj     | mkrefany | newarr  | refanyval |
    // sizeof  | stobj     | unbox

    emit(opcode, arg)
  }

  /**
   * Puts a call or callvirt instruction onto the Microsoft intermediate
   * language (CIL) stream.
   */
  def emitCall(opcode: OpCode, arg: MethodInfo,
    optionalParameterTypes: Array[Type]): Unit = {
    assert(arg != null)
    // pop size is the number of parameters
    // push size is either 0 (void Method) either 1
    //System.out.println(arg.ReturnType.Size + " " + arg.GetParameters().length);
    emit(opcode, arg, (if (arg.returnType == Common.SystemVoid) 0 else 1) -
      arg.getParameters.length)
  }

  /**
   * Emits the Common Intermediate Language (CIL) necessary to
   * call WriteLine with the given field.
   */
  def emitWriteLine(arg: FieldInfo): Unit = {
    // first load field info
    // if static use OpCode.Ldsfld
    if (arg.isStatic)
      emit(OpCode.Ldsfld, arg)
    else
      emit(OpCode.Ldfld, arg)
    // then call System.Console.WriteLine(arg.Type)
    val m: MethodInfo = Common.SystemConsole.getMethod("WriteLine", Array(arg.fieldType))
    emitCall(OpCode.Call, m, null)
  }

  /**
   * Emits the Common Intermediate Language (CIL) necessary
   * to call WriteLine with the given local variable.
   */
  def emitWriteLine(arg: LocalBuilder): Unit = {
    // first load local variable
    emit(OpCode.Ldloc, arg)
    // then call System.Console.WriteLine(arg.Type)
    val m: MethodInfo = Common.SystemConsole.getMethod("WriteLine", Array(arg.LocalType))
    emitCall(OpCode.Call, m, null)
  }

  /**
   * Emits the Common Intermediate Language (CIL) to call
   * WriteLine with a string.
   */
  def emitWriteLine(arg: String): Unit = {
    // first load string
    emit(OpCode.Ldstr, arg)
    // then call System.Console.WriteLine(string)
    val m: MethodInfo = Common.SystemConsole.getMethod("WriteLine", Array(Common.SystemString))
    emitCall(OpCode.Call, m, null)
  }

  /**
   * Declares a local variable.
   */
  def declareLocal(localType: Type): LocalBuilder = {
    val l: LocalBuilder = new LocalBuilder(locals, localType)
    locals = locals + 1
    localList += l
    return l
  }

  /**
   * Returns a new label that can be used as a token for branching.
   * In order to set the position of the label within the stream, you
   * must call MarkLabel. This is just a token and does not yet represent
   * any particular location within the stream.
   */
  def defineLabel(): Label = {
    new Label.NormalLabel()
  }

  /**
   * Marks the Common Intermediate Language (CIL) stream's
   * current position with the given label.
   */
  def markLabel(label: Label): Unit = {
    label.mergeWith(lastLabel)
    /*
	label.address = lastLabel.address;
	//label.stacksize = lastLabel.stacksize;
	if (label.stacksize >= 0)
	    lastLabel.stacksize = label.stacksize;
	*/
  }

  /** Begins a lexical scope. */
  def beginScope(): Unit = {
    emitSpecialLabel(Label.NewScope)
  }

  /** Ends a lexical scope. */
  def endScope(): Unit = {
    emitSpecialLabel(Label.EndScope)
  }

  /**
   * Begins an exception block for a non-filtered exception.
   * The label for the end of the block. This will leave you in the correct
   * place to execute finally blocks or to finish the try.
   */
  def beginExceptionBlock(): Unit = {
    emitSpecialLabel(Label.Try)
    val endExc: Label = new Label.NormalLabel() // new Label(lastLabel) ???
    excStack.push(Label.Try, endExc)
  }

  /** Begins a catch block. */
  def beginCatchBlock(exceptionType: Type): Unit = {
    val kind = excStack.peekKind()
    if (kind == Label.Kind.Try ||
      kind == Label.Kind.Catch) {
      /* ok */
    } else {
      throw new RuntimeException("Catch should follow either a try or catch")
    }
    val endExc: Label = excStack.popLabel()
    emit(OpCode.Leave, endExc)
    // the CLI automatically provide the exception object on the evaluation stack
    // we adjust the stacksize
    lastLabel.incStacksize()
    excStack.push(Label.Catch, endExc)
    emitSpecialLabel(Label.Catch, exceptionType)
  }

  /** Ends an exception block. */
  def endExceptionBlock(): Unit = {
    val kind = excStack.peekKind()
    if (kind == Label.Kind.Try) {
      throw new RuntimeException("Try block with neither catch nor finally")
    } else if (kind == Label.Kind.Catch) {
      emit(OpCode.Leave, excStack.peekLabel())
    } else if (kind == Label.Kind.Finally) {
      emit(OpCode.Endfinally)
    }
    markLabel(excStack.popLabel())
    emitSpecialLabel(Label.EndTry)
  }

  /**
   * Begins a finally block in the Common Intermediate Language
   * (CIL) instruction stream.
   */
  def beginFinallyBlock(): Unit = {
    val endExc: Label = excStack.popLabel()
    emit(OpCode.Leave, endExc)
    excStack.push(Label.Finally, endExc)
    emitSpecialLabel(Label.Finally)
  }

  /**
   * Emits an instruction to throw an exception.
   */
  def throwException(exceptionType: Type): Unit = {
    assert(exceptionType != null)
    if (!exceptionType.isSubtypeOf(Common.SystemException))
      throw new RuntimeException
    (exceptionType + " doesn't extend System.Exception")
    val ctor: ConstructorInfo = exceptionType.getConstructor(Type.EmptyTypes)
    if (ctor == null)
      throw new RuntimeException("Type " + exceptionType
        + "doesn't have a default constructor")
    emit(OpCode.Newobj, ctor)
    emit(OpCode.Throw)
  }

  /**
   * sets the line of the source file corresponding to the next instruction
   */
  def setPosition(line: Int): Unit = {
    if (line != 0) lineNums.put(lastLabel, Integer.toString(line))
  }

  def setPosition(line: Int, filename: String): Unit = {
    if (line != 0) lineNums.put(lastLabel, line + "  '" + filename + "'")
  }

  def setPosition(startLine: Int, endLine: Int, startCol: Int, endCol: Int, filename: String): Unit = {
    val lineRange = startLine + "," + endLine
    val colRange = startCol + "," + endCol
    lineNums.put(lastLabel, lineRange + ":" + colRange + "  '" + filename + "'")
  }

  def getLocals: Array[LocalBuilder] = localList.toArray

  def getLabelIterator = labelList.iterator

  def getOpcodeIterator = opcodeList.iterator

  def getArgumentIterator = argumentList.iterator

  //##########################################################################
  // private implementation details

  // the local variable list
  private final val localList = scala.collection.mutable.ArrayBuffer.empty[LocalBuilder]

  // the label list, the opcode list and the opcode argument list
  // labelList is an array of Label
  // opcodeList is an array of OpCode
  // argumentList is an array of Object (null if no argument)
  private final val labelList = scala.collection.mutable.ArrayBuffer.empty[Label]
  private final val opcodeList = scala.collection.mutable.ArrayBuffer.empty[OpCode]
  private final val argumentList = scala.collection.mutable.ArrayBuffer.empty[Object]

  // the program counter (pc)
  // also called the stream's current position
  private var pc: Int = 0

  // last label
  private var lastLabel: Label = new Label.NormalLabel(pc, 0)

  // the maximum size of stack
  private var maxstack: Int = 0

  // the number of the locals
  private var locals: Int = 0

  // stack of label for exception mechanism
  private val excStack: ExceptionStack = new ExceptionStack()

  val lineNums = scala.collection.mutable.Map.empty[Label, String]

  def getMaxStacksize: Int = { this.maxstack }

  // private emit with Object Argument
  private def emit(opcode: OpCode, arg: Object): Unit = {
    emit(opcode, arg, opcode.CEE_popush)
  }

  // private emit with Object Argument and override POPUSH
  private def emit(opcode: OpCode, arg: Object, overridePOPUSH: Int): Unit = {
    // add label, opcode and argument
    labelList += lastLabel
    opcodeList += opcode
    argumentList += arg
    // compute new lastLabel (next label)
    val stackSize: Int = lastLabel.getStacksize + overridePOPUSH
    if (stackSize < 0) {
      val msg = "ILGenerator.emit(): Stack underflow in method: " + owner
      scala.Console.println(msg)
      // throw new RuntimeException(msg)
    }
    if (stackSize > maxstack)
      maxstack = stackSize
    var address: Int = lastLabel.getAddress + opcode.CEE_length
    if (opcode.CEE_opcode == OpCode.CEE_SWITCH) {
      address = address + 4 * arg.asInstanceOf[Array[Label]].length
    }
    lastLabel = new Label.NormalLabel(address, stackSize)
    pc = pc + 1
  }

  def ldarg0WasJustEmitted(): Boolean = {
    if (opcodeList.isEmpty)
      return false
    val lastEmitted = opcodeList(opcodeList.size - 1)
    lastEmitted eq OpCode.Ldarg_0
  }

  private def emitSpecialLabel(l: Label): Unit = {
    emitSpecialLabel(l, null)
  }
  private def emitSpecialLabel(l: Label, catchType: Type): Unit = {
    labelList += l
    opcodeList += null
    argumentList += catchType
  }

  //##########################################################################
  //
  @throws(classOf[IOException])
  def apply(v: Visitor): Unit = {
    v.caseILGenerator(this)
  }

  //##########################################################################
} // class ILGenerator

object ILGenerator {
  val NO_LABEL: String = ""

  private final class ExceptionStack {
    private val labels = new scala.collection.mutable.Stack[Label]()
    private val kinds = new scala.collection.mutable.Stack[Label]()
    def ExceptionStack(): Unit = {}
    def pop(): Unit = { labels.pop; kinds.pop }
    def push(kind: Label, label: Label): Unit = {
      kinds.push(kind); labels.push(label)
    }
    def peekKind(): Label.Kind = kinds.top.getKind
    def peekLabel(): Label = labels.top
    def popLabel(): Label = { kinds.pop(); labels.pop() }
  }

}

