import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/* Chapter 1 - Preliminaries */

/** CHAPTER 4 */

abstract class Type

trait TypeCheckable {
  import Lint._
  import Lvar._

  def typeOfOp(op: PrimOp): (List[Type], Type)
  def typeOfExpr(e: Expr, tnv: List[(String, Type)]): Type
}

object Lif {

  import Library.{SExpr}
  import Library.Reader.{read}
  import Lint._
  import Lvar._

  case class And() extends PrimOp
  case class Or() extends PrimOp
  case class Not() extends PrimOp
  case class Eq() extends PrimOp
  case class Lt() extends PrimOp
  case class LEq() extends PrimOp
  case class Gt() extends PrimOp
  case class GEq() extends PrimOp

  case class Bool(b: Boolean) extends Expr
  case class If(c: Expr, t: Expr, e: Expr) extends Expr

  abstract class Lif
  case class Program(body: Expr) extends Lif

  trait Parsable
    extends ParseExpr[Expr]
      with Lvar.Parsable {
    import Library._

    override def reservedWords = super.reservedWords ++ List(
      "if", "and", "or", "not", "#t", "#f", "<", "<=", ">", ">=", "eq?"
    )

    override def parseExpr(s: SExpr): Expr = s match {
      case SList(List(SSym("if"), ec, et, ee)) =>
        If(parseExpr(ec), parseExpr(et), parseExpr(ee))
      case SList(List(SSym("and"), e1, e2)) =>
        Prim(And(), List(parseExpr(e1), parseExpr(e2)))
      case SList(List(SSym("or"), e1, e2)) =>
        Prim(Or(), List(parseExpr(e1), parseExpr(e2)))
      case SList(List(SSym("not"), e)) =>
        Prim(Not(), List(parseExpr(e)))
      case SList(List(SSym("eq?"), e1, e2)) =>
        Prim(Eq(), List(parseExpr(e1), parseExpr(e2)))
      case SList(List(SSym("<"), e1, e2)) =>
        Prim(Lt(), List(parseExpr(e1), parseExpr(e2)))
      case SList(List(SSym("<="), e1 ,e2)) =>
        Prim(LEq(), List(parseExpr(e1), parseExpr(e2)))
      case SList(List(SSym(">"), e1 ,e2)) =>
        Prim(Gt(), List(parseExpr(e1), parseExpr(e2)))
      case SList(List(SSym(">="), e1 ,e2)) =>
        Prim(GEq(), List(parseExpr(e1), parseExpr(e2)))
      case SSym("true") => Bool(true)
      case SSym("false") => Bool(false)
      case _ => super.parseExpr(s)
    }
  }

  object Parser extends Parsable {
    def toLif(s: String): Lif = parseLif(read(s))
    def parseLif(s: SExpr): Lif = Program(parseExpr(s))
  }



  case class IntT() extends Type
  case class BoolT() extends Type

  object TypeChecker extends TypeCheckable {
    case class TypeException(msg: String) extends RuntimeException(msg)

    override def typeOfOp(op: PrimOp): (List[Type], Type) = op match {
      case Plus() => (List(IntT(), IntT()), IntT())
      case Minus() => (List(IntT(), IntT()), IntT())
      case Read() => (Nil, IntT())
      case And() => (List(BoolT(), BoolT()), BoolT())
      case Or() => (List(BoolT(), BoolT()), BoolT())
      case Not() => (List(BoolT()), BoolT())
      case Lt() => (List(IntT(), IntT()), BoolT())
      case LEq() => (List(IntT(), IntT()), BoolT())
      case Gt() => (List(IntT(), IntT()), BoolT())
      case GEq() => (List(IntT(), IntT()), BoolT())
    }

    override def typeOfExpr(e: Expr, tnv: List[(String, Type)]): Type = e match {
      case Prim(Eq(), List(e1, e2)) =>
        val (t1, t2) = (typeOfExpr(e1, tnv), typeOfExpr(e2, tnv))
        if (t1 == t2) BoolT()
        else
          throw TypeException(
            "Type error: expected sub-expressions of eq? expression to have the same type, but found\n  [" + t1 + ", " + t2 + "]"
          )
      case Prim(Minus(), List(e1)) =>
        val t1 = typeOfExpr(e1, tnv)
        if (t1 != IntT()) throw TypeException("Type error: Negated non-int")
        IntT()
      case Prim(Print(), List(e1)) =>
        typeOfExpr(e1, tnv)
      case Bool(_) => BoolT()
      case If(c, t, e) =>
        val cty = typeOfExpr(c, tnv)
        if (cty == BoolT()) {
          val (ty1, ty2) = (typeOfExpr(t, tnv), typeOfExpr(e, tnv))
          if (ty1 == ty2)
            ty1
          else
            throw TypeException(
              "Type error: expected branches of if " +
                "expression to have the same type, but found\n  [" + ty1 + ", " + ty2 + "]"
            )
        } else throw TypeException(
          "Type error: first sub-expression of if must be bool typed. Found\n  " + cty
        )
      case Num(_) => IntT()
      case Prim(op, es) =>
        val (pts, rt) = typeOfOp(op)
        val ats = es.map({ e => typeOfExpr(e, tnv) })
        if (ats == pts)
          rt
        else
          throw TypeException(
            "Type error: expected sub-expresssions of " + op +
              " to have types\n  [" + pts.mkString(", ") + "]\nbut found\n  [" +
              ats.mkString(", ") + "]."
          )
      case Var(x) => tnv.find({ case (y, _) => x == y }) match {
        case None => throw TypeException(
          "Type error: the variable " + x + " occurs free!")
        case Some((_, t)) => t
      }
      case Let(x, e, body) =>
        typeOfExpr(body, (x, typeOfExpr(e, tnv)) :: tnv)
    }

    def typeOfProg(p: Lif): Type = p match {
      case Program(e) => typeOfExpr(e, Nil)
    }
  }

  case class BoolV(n: Boolean) extends InterpreterValue

  trait Interpretable
    extends Lvar.Interpretable {

    override def interpExpr(e: Expr, nv: List[(String, InterpreterValue)], inputs: ListBuffer[Long], outputs: ListBuffer[Long]): InterpreterValue = e match {
      case Bool(b) => BoolV(b)
      case Prim(And(), List(e1, e2)) => if(interpExpr(e1, nv, inputs, outputs).bool()) interpExpr(e2, nv, inputs, outputs) else BoolV(false)
      case Prim(Or(), List(e1, e2)) => if(interpExpr(e1, nv, inputs, outputs).bool()) BoolV(true) else interpExpr(e2, nv, inputs, outputs)
      case Prim(Not(), List(e1)) => BoolV(!interpExpr(e1, nv, inputs, outputs).bool())
      case Prim(Eq(), List(e1, e2)) => BoolV(interpExpr(e1, nv, inputs, outputs) == interpExpr(e2, nv, inputs, outputs))
      case Prim(Lt(), List(e1, e2)) => BoolV(interpExpr(e1, nv, inputs, outputs).num() < interpExpr(e2, nv, inputs, outputs).num())
      case Prim(LEq(), List(e1, e2)) => BoolV(interpExpr(e1, nv, inputs, outputs).num() <= interpExpr(e2, nv, inputs, outputs).num())
      case Prim(Gt(), List(e1, e2)) => BoolV(interpExpr(e1, nv, inputs, outputs).num() > interpExpr(e2, nv, inputs, outputs).num())
      case Prim(GEq(), List(e1, e2)) => BoolV(interpExpr(e1, nv, inputs, outputs).num() >= interpExpr(e2, nv, inputs, outputs).num())
      case If(e1, e2, e3) => if(interpExpr(e1, nv, inputs, outputs).bool()) interpExpr(e2, nv, inputs, outputs) else interpExpr(e3, nv, inputs, outputs)
      case _ => super.interpExpr(e, nv, inputs, outputs)
    }
  }

  object Interpreter extends Interpretable {
    def interpLif(p: Lif, inputs: ListBuffer[Long], outputs: ListBuffer[Long]): Long = p match {
      case Program(e) => interpExpr(e, Nil, inputs, outputs).num()
    }
  }
}

object Cif {

  import Cvar._
  import Lint._

  case class Goto(label: String) extends Tail
  case class IfStmt(cmp: Expr, if_branch: Goto, else_branch: Goto) extends Tail

  sealed abstract class Cif
  case class CProgram(blocks: List[(String, Tail)]) extends Cif

  trait Interpretable
    extends Lint.Interpretable
      with Lvar.Interpretable
      with Lif.Interpretable
      with Cvar.Interpretable {

    override def interpTail(t: Tail, nv: List[(String, InterpreterValue)], inputs: ListBuffer[Long], outputs: ListBuffer[Long], blocks: List[(String, Tail)]): InterpreterValue = t match {
      case Goto(l) => interpTail(blocks.find{ case (n, _) => n == l }.get._2, nv, inputs, outputs, blocks)
      case IfStmt(c, e1, e2) => if (interpExpr(c, nv, inputs, outputs).bool()) interpTail(e1, nv, inputs, outputs, blocks) else interpTail(e2, nv, inputs, outputs, blocks)
      case t => super.interpTail(t, nv, inputs, outputs, blocks)
    }
  }

  object Interpreter extends Interpretable {
    def interpCif(p: Cif, inputs: ListBuffer[Long], outputs: ListBuffer[Long]): Long = p match {
      case CProgram(blocks@((_, t) :: _)) => interpTail(t, Nil, inputs, outputs, blocks).num()
    }
  }
}

