import Lfun.FunctionDef
import Library.SMulti

import scala.collection.mutable.ListBuffer

object Lfun {

  import Library.{SExpr}
  import Library.Reader.{read}
  import Lint._
  import Lvar._
  import Lif._

  case class Apply(f: Expr, args: List[Expr]) extends Expr
  case class FunRef(f: String, n: Int) extends Expr

  abstract class Def
  case class FunctionDef(name: String, args: List[(String, Type)], rtrn: Type, body: Expr) extends Def

  case class FunT(args: List[Type], rtrn: Type) extends Type

  abstract class Lfun
  case class ProgramDefsExp(defs: List[Def], body: Expr) extends Lfun
  case class ProgramDefs(defs: List[Def]) extends Lfun

  trait Parsable
    extends ParseExpr[Expr]
      with Lif.Parsable {
    import Library._

    override def reservedWords = super.reservedWords ++ List(
      "define"
    )

    def isExpr(s: SExpr): Boolean =
      try {
        parseExpr(s)
        true
      } catch {
        case _: ParseException => false
      }

    override def parseExpr(s: SExpr): Expr = s match {
      case SList(h :: tail) if isExpr(h) =>
        Apply(parseExpr(h), tail.map(parseExpr))
      case _ => super.parseExpr(s)
    }

    def parseType(s: SExpr): Type = s match {
      case SSym("Integer") => IntT()
      case SSym("Boolean") => BoolT()
      case SList(things) if things.length >= 3 => {
        assert(things(things.length - 2) == SSym("->"))
        FunT(things.slice(0, things.length - 2).map(parseType), parseType(things.last))
      }
      case _ => throw ParseException("Type", s)
    }

    def parseDef(s: SExpr): Def = s match {
      case SList(List(SSym("define"), SSym(name), SList(args), rtrn, body)) =>
        FunctionDef(name, args.map{
          case SList(List(SSym(aname), SSym(":"), typ)) => (aname, parseType(typ))
        }, parseType(rtrn), parseExpr(body))
      case _ => throw ParseException("Definition", s)
    }
  }

  object Parser extends Parsable {
    def toLfun(s: String): Lfun = parseLfun(read(s))

    def parseLfun(s: SExpr): Lfun = s match {
      case SMulti(list) => ProgramDefsExp(list.slice(0, list.length - 1).map(parseDef), parseExpr(list.last))
      case s => ProgramDefsExp(List(), parseExpr(s))
    }
  }

  case class FunV(n: FunctionDef) extends InterpreterValue

  trait Interpretable
    extends Lif.Interpretable {

    def interpDef(d: Def): (String, InterpreterValue) = d match {
      case fd@FunctionDef(name, _, _, _) => (name, FunV(fd))
    }

    override def interpExpr(e: Expr, nv: List[(String, InterpreterValue)], inputs: ListBuffer[Long], outputs: ListBuffer[Long]): InterpreterValue = e match {
      case FunRef(f, _) => interpExpr(Var(f), nv, inputs, outputs)
      case Apply(f, args) => {
        val fd: FunctionDef = interpExpr(f, nv, inputs, outputs).fun()
        assert(fd.args.length == args.length)
        interpExpr(fd.body, fd.args.zip(args.map(a => interpExpr(a, nv, inputs, outputs))).map{
          case ((aname, _), aval) => (aname, aval)
        } ++ nv, inputs, outputs)
      }
      case _ => super.interpExpr(e, nv, inputs, outputs)
    }
  }

  object Interpreter extends Interpretable {
    def interpLfun(p: Lfun, inputs: ListBuffer[Long], outputs: ListBuffer[Long]): Long = p match {
      case ProgramDefsExp(ds, e) => interpExpr(e, ds.map(interpDef), inputs, outputs).num()
      case ProgramDefs(ds) => interpExpr(ds.collect{case FunctionDef("main", _, _, e) => e}.head, ds.map(interpDef), inputs, outputs).num()
    }
  }
}

object Cfun {

  import Cvar._
  import Lint._
  import Cif._
  import Lfun._

  abstract class CDef
  case class CFunDef(name: String, args: List[(String, Type)], rtrn: Type, blocks: List[(String, Tail)]) extends CDef

  sealed abstract class Cfun
  case class CProgram(defs: List[CDef]) extends Cfun

  case class FunV(n: FunctionDef) extends InterpreterValue

  trait Interpretable
    extends Cif.Interpretable
      with Lfun.Interpretable {
    override def interpExpr(e: Expr, nv: List[(String, InterpreterValue)], inputs: ListBuffer[Long], outputs: ListBuffer[Long]): InterpreterValue = e match {
      case Apply(f, args) => {
        val fd: CFunDef = interpExpr(f, nv, inputs, outputs).cfun()
        assert(fd.args.length == args.length)
        interpTail(fd.blocks.head._2, fd.args.zip(args.map(a => interpExpr(a, nv, inputs, outputs))).map {
          case ((aname, _), aval) => (aname, aval)
        } ++ nv, inputs, outputs, fd.blocks)
      }
      case _ => super.interpExpr(e, nv, inputs, outputs)
    }
  }

  case class CFunV(n: CFunDef) extends InterpreterValue

  object Interpreter extends Interpretable {
    def interpCfun(p: Cfun, inputs: ListBuffer[Long], outputs: ListBuffer[Long]): Long = p match {
      case CProgram(defs) =>
        //Find main function
        val main = defs.collect { case c@CFunDef("main", _, _, _) => c }.head
        //Put all other functions in the environment
        val funs = defs.map { case c@CFunDef(name, _, _, _) => (name, CFunV(c)) }
        //Interpret
        interpTail(main.blocks.head._2, funs, inputs, outputs, main.blocks).num()
    }
  }
}