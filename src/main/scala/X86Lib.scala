import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object X86int {

  sealed abstract class Register
  case class RSP() extends Register
  case class RBP() extends Register
  case class RAX() extends Register
  case class RBX() extends Register
  case class RCX() extends Register
  case class RDX() extends Register
  case class RSI() extends Register
  case class RDI() extends Register
  case class R8()  extends Register
  case class R9()  extends Register
  case class R10() extends Register
  case class R11() extends Register
  case class R12() extends Register
  case class R13() extends Register
  case class R14() extends Register
  case class R15() extends Register

  abstract class Arg
  case class Imm(i: Long)                extends Arg
  case class Reg(r: Register)            extends Arg
  case class Deref(r: Register, i: Long) extends Arg

  sealed abstract class Cmd
  case class Addq()  extends Cmd
  case class Subq()  extends Cmd
  case class Negq()  extends Cmd
  case class Movq()  extends Cmd
  case class Pushq() extends Cmd
  case class Popq()  extends Cmd

  sealed abstract class Instruction
  case class Instr(c: Cmd, args: List[Arg])   extends Instruction
  case class Callq(l: String, arg_count: Int) extends Instruction
  case class Retq()                           extends Instruction
  case class Jmp(l: String)                   extends Instruction

  class BlockInfo

  sealed abstract class Block
  case class Blk(info: BlockInfo, instrs: List[Instruction]) extends Block

  class Info
  case class EmptyInfo() extends Info
  case class ProgramInfo(locals: List[String], stackSpace: Long) extends Info

  sealed abstract class X86int
  case class X86IntProgram(info: Info, blocks: List[(String, Block)]) extends X86int
}

object X86var {

  import X86int._

  case class XVar(x: String) extends Arg

  sealed abstract class X86var
  case class X86VarProgram(info: Info, blocks: List[(String, Block)]) extends X86var


}

object X86if {

  import X86int._

  case class Xorq() extends Cmd
  case class Cmpq() extends Cmd
  case class Movzbq() extends Cmd

  abstract class CmpFlag
  case class LT() extends CmpFlag
  case class LE() extends CmpFlag
  case class EQ() extends CmpFlag
  case class GE() extends CmpFlag
  case class GT() extends CmpFlag

  abstract class ByteReg
  case class AH() extends ByteReg
  case class AL() extends ByteReg
  case class BH() extends ByteReg
  case class BL() extends ByteReg
  case class CH() extends ByteReg
  case class CL() extends ByteReg
  case class DH() extends ByteReg
  case class DL() extends ByteReg

  case class ByteRegister(r: ByteReg) extends Arg

  case class JmpIf(cmp: CmpFlag, l: String) extends Instruction
  case class SetIf(cmp: CmpFlag, reg8: ByteReg) extends Instruction

  sealed abstract class X86if
  case class X86IfProgram(info: Info, blocks: List[(String, Block)]) extends X86if

}

object X86fun {

  import X86int._

  case class Leaq(lbl: String, arg: Arg) extends Instruction
  case class CallqIndirect(function: Arg, arg_count: Int) extends Instruction

  abstract class XDef
  case class XFunDef(name: String, args: List[(String, Type)], rtrn: Type, body: List[(String, Block)]) extends XDef

  sealed abstract class X86fun
  case class X86FunProgram(info: Info, blocks: List[XDef]) extends X86fun

}

object X86Interpreter {
  import X86int._
  import X86var._
  import X86if._
  import X86fun._

  class InterpreterState(val blocks: List[(String, Block)],
                         val inputs: ListBuffer[Long],
                         val outputs: ListBuffer[Long],
                         val registers: mutable.HashMap[Register, Long] = mutable.HashMap(),
                         val variables: mutable.HashMap[String, Long] = mutable.HashMap(),
                         val memory: mutable.HashMap[Long, Long] = mutable.HashMap(),
                         var cmp_register: Int = 0, // -1 0 1
                         val call_stack: ListBuffer[Long] = ListBuffer(),
                         val strict: Boolean) {



    def read(arg: Arg): Long = arg match {
      case Imm(v) => v
      case Reg(reg) => this.registers(reg)
      case Deref(reg, offset) =>
        val addr = this.registers(reg) + offset
        assert(addr % 8 == 0, "Offset reads not supported")
        if (strict) assert(addr >= registers(RSP()), "Write outside of stack")
        this.memory(addr)
      case XVar(name) => this.variables(name)
      case ByteRegister(reg) => reg match {
        case AL() => this.registers(RAX()).toByte
        case BL() => this.registers(RBX()).toByte
        case CL() => this.registers(RCX()).toByte
        case DL() => this.registers(RDX()).toByte
        case AH() => (this.registers(RAX()) >> 8).toByte
        case BH() => (this.registers(RBX()) >> 8).toByte
        case CH() => (this.registers(RCX()) >> 8).toByte
        case DH() => (this.registers(RDX()) >> 8).toByte
      }
    }

    def write(arg: Arg, w: Long): Unit = arg match {
      case Imm(_) => assert(false, "Write to immediate value")
      case Reg(reg) => this.registers(reg) = w
      case Deref(reg, offset) =>
        val addr = this.registers(reg) + offset
        assert(addr % 8 == 0, "Offset reads not supported")
        if (strict) assert(addr >= registers(RSP()), "Write outside of stack")
        this.memory(addr) = w
      case XVar(name) => this.variables(name) = w
      case ByteRegister(reg) => reg match {
        case AL() => this.registers(RAX()) = (w & 0xFF) | (this.registers.getOrElse(RAX(), 0L) & 0xFFFFFFFFFFFF00L)
        case BL() => this.registers(RBX()) = (w & 0xFF) | (this.registers.getOrElse(RBX(), 0L) & 0xFFFFFFFFFFFF00L)
        case CL() => this.registers(RCX()) = (w & 0xFF) | (this.registers.getOrElse(RCX(), 0L) & 0xFFFFFFFFFFFF00L)
        case DL() => this.registers(RDX()) = (w & 0xFF) | (this.registers.getOrElse(RDX(), 0L) & 0xFFFFFFFFFFFF00L)
        case AH() => this.registers(RAX()) = ((w & 0xFF) << 8) | (this.registers.getOrElse(RAX(), 0L) & 0xFFFFFFFFFF00FFL)
        case BH() => this.registers(RBX()) = ((w & 0xFF) << 8) | (this.registers.getOrElse(RBX(), 0L) & 0xFFFFFFFFFF00FFL)
        case CH() => this.registers(RCX()) = ((w & 0xFF) << 8) | (this.registers.getOrElse(RCX(), 0L) & 0xFFFFFFFFFF00FFL)
        case DH() => this.registers(RDX()) = ((w & 0xFF) << 8) | (this.registers.getOrElse(RDX(), 0L) & 0xFFFFFFFFFF00FFL)
      }
    }

    def runAddress(address: Long): Boolean = {
      val blockId: Long = address >> 32
      val instrId: Long = address & 0xFFFFFFFF

      val blk = this.blocks(blockId.toInt)._2
      val blk2 = blk match {
        case Blk(info, instrs) => Blk(info, instrs.slice(instrId.toInt, instrs.length))
      }

      runBlock(blockId, blk2)
    }

    def runBlock(blockId: Long, block: Block): Boolean = block match {
      case Blk(inf, Instr(Addq(), List(a1, a2)) :: instrs) =>
        write(a2, read(a1) + read(a2))
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Instr(Subq(), List(a1, a2)) :: instrs) =>
        write(a2, read(a2) - read(a1))
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Instr(Negq(), List(a)) :: instrs) =>
        write(a, -read(a))
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Instr(Movq(), List(a1, a2)) :: instrs) =>
        write(a2, read(a1))
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Instr(Movzbq(), List(a1@ByteRegister(_), a2@(Reg(_) | XVar(_)))) :: instrs) =>
        write(a2, read(a1))
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Instr(Xorq(), List(a1, a2)) :: instrs) =>
        write(a2, read(a1) ^ read(a2))
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Instr(Pushq(), List(a)) :: instrs) =>
        this.registers(RSP()) -= 8
        val addr = this.registers(RSP())
        assert(addr % 8 == 0, "Offset reads not supported")
        this.memory(addr) = read(a)
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Instr(Popq(), List(a)) :: instrs) =>
        val addr = this.registers(RSP())
        assert(addr % 8 == 0, "Offset reads not supported")
        this.registers(RSP()) += 8
        write(a, this.memory(addr))
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Callq("_read_int", 0) :: instrs) =>
        this.registers(RAX()) = this.inputs.remove(0)
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Callq("_print_int", 1) :: instrs) =>
        this.outputs += this.registers(RDI())
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Leaq(l, v) :: instrs) =>
        val subBlockId: Long = this.blocks.indexWhere(b => b._1 == l)
        write(v, subBlockId << 32)
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Callq(l, _) :: instrs) =>
        this.registers(RSP()) -= 8

        val instrId: Long = this.blocks(blockId.toInt)._2 match { case Blk(info, blkinstrs) => blkinstrs.length - instrs.length }
        this.memory(this.registers(RSP())) = blockId << 32 | instrId
        this.call_stack += this.registers(RSP())

        val subBlockId = this.blocks.indexWhere(b => b._1 == l)
        runBlock(subBlockId, this.blocks(subBlockId)._2)
      case Blk(inf, CallqIndirect(v, _) :: instrs) =>
        this.registers(RSP()) -= 8

        val instrId: Long = this.blocks(blockId.toInt)._2 match {case Blk(info, blkinstrs) => blkinstrs.length - instrs.length}
        this.memory(this.registers(RSP())) = blockId << 32 | instrId
        this.call_stack += this.registers(RSP())

        val subAddress: Long = read(v)
        runAddress(subAddress)
      case Blk(inf, Retq() :: _) =>
        if (this.registers(RSP()) == Long.MaxValue - 7) {
          return true
        } else {
          val rsp = this.call_stack.remove(this.call_stack.length - 1)
          assert(rsp == this.registers(RSP()))

          val returnAddress: Long = this.memory(this.registers(RSP()))
          this.registers(RSP()) += 8

          runAddress(returnAddress)
        }
      case Blk(inf, Jmp(l) :: _) =>
        val blockId = this.blocks.indexWhere(b => b._1 == l)
        runBlock(blockId, this.blocks(blockId)._2)
      case Blk(inf, Instr(Cmpq(), List(a1, a2)) :: instrs) =>
        this.cmp_register = read(a2).compare(read(a1))
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, JmpIf(cmp, l) :: instrs) =>
        if (cmp match {
          case LT() if this.cmp_register < 0 => true
          case LE() if this.cmp_register <= 0 => true
          case EQ() if this.cmp_register == 0 => true
          case GE() if this.cmp_register >= 0 => true
          case GT() if this.cmp_register > 0 => true
          case _ => false
        }) {
          val blockId = this.blocks.indexWhere(b => b._1 == l)
          runBlock(blockId, this.blocks(blockId)._2)
        } else {
          runBlock(blockId, Blk(inf, instrs))
        }
      case Blk(inf, SetIf(cmp, br) :: instrs) =>
        write(ByteRegister(br), cmp match {
          case LT() if this.cmp_register < 0 => 1
          case LE() if this.cmp_register <= 0 => 1
          case EQ() if this.cmp_register == 0 => 1
          case GE() if this.cmp_register >= 0 => 1
          case GT() if this.cmp_register > 0 => 1
          case _ => 0
        })
        runBlock(blockId, Blk(inf, instrs))
      case Blk(inf, Nil) =>
        return false
    }

  }
}