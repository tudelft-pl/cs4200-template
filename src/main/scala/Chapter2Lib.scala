import Cfun.{CFunDef, CFunV}
import Cvar.Tail
import Lfun.{FunV, FunctionDef}
import Lif.BoolV
import Lint.InterpreterValue

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/* Chapter 1 - Preliminaries */

object Library {

  import scala.util.parsing.input.Positional
  import scala.util.parsing.combinator._

  sealed abstract class SExpr extends Positional
  case class SList(list: List[SExpr])  extends SExpr
  case class SSym(symbol: String)      extends SExpr
  case class SNum(num: Long)           extends SExpr
  case class SString(string : String)  extends SExpr
  case class SMulti(list: List[SExpr]) extends SExpr

  case class ReaderException(msg:String) extends RuntimeException

  object Reader extends JavaTokenParsers {
    def read(text: String): SExpr = {
      val result = parseAll(sexpr, text)
      result match {
        case Success(r, _) => r
        case Failure(msg, n) =>
          throw ReaderException(msg + " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
        case Error(msg, n) =>
          throw ReaderException(msg + " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
      }
    }

    def sexpr: Parser[SExpr] = positioned(multi | sexpr2)
    def multi: Parser[SExpr] = sexpr2.+ ^^ (s => if (s.length > 1) SMulti(s) else s.head)

    def sexpr2: Parser[SExpr] = positioned(num | string | slist | symbol)
    def symbol: Parser[SExpr] = """[^)"\s]+""".r ^^ SSym
    def string : Parser[SExpr] =
      "\"" ~> """([^"\p{Cntrl}\\]|\\[\\'"bfnrt])*""".r <~ "\"" ^^ SString
    def slist: Parser[SExpr] = "(" ~> sexpr2.* <~ ")" ^^ SList
    def num: Parser[SExpr] = wholeNumber ^^ { s => SNum(s.toLong) }
  }

  case class ParseException(kind: String, s: SExpr)
    extends RuntimeException(
      "Unrecognized " + kind + ":\n  "
        + s
        + "\nat line "
        + s.pos.line
        + ", column "
        + s.pos.column )

  case class ReservedWordException(word: String, s: SExpr)
    extends RuntimeException(
      "Found reserved word where identifier was expected:\n  "
        + word
        + "\nat line "
        + s.pos.line
        + ", column "
        + s.pos.column )


}

// Interfaces and shorthands

trait ParseExpr[Expr] {
  import Library._
  def reservedWords: List[String]
  def parseExpr(e: SExpr): Expr
}

object Lint {

  abstract class PrimOp
  case class Plus()  extends PrimOp
  case class Minus() extends PrimOp
  case class Read()  extends PrimOp

  abstract class Expr
  case class Num(i: Long)    extends Expr
  case class Prim(op: PrimOp, es: List[Expr]) extends Expr

  abstract class Lint
  case class Program(body: Expr) extends Lint

  trait Parsable extends ParseExpr[Expr] {
    import Library._
    import Reader._

    override def reservedWords: List[String] =
      List("read", "+", "-")

    //TODO your week 1 solution here
    override def parseExpr(s: SExpr): Expr = ???

    def toExpr(s: String): Expr = parseExpr(read(s))
  }

  object Parser extends Parsable {
    import Library._
    import Reader._

    def toLint(s: String): Lint = parseLint(read(s))

    def parseLint(s: SExpr): Lint = Program(parseExpr(s))
  }

  abstract class InterpreterValue {
    def num(): Long = this match {
      case NumV(n) => n
      case BoolV(n) => if (n) 1 else 0
    }

    def bool(): Boolean = this match {
      case BoolV(n) => n
    }

    def fun(): FunctionDef = this match {
      case FunV(n) => n
    }

    def cfun(): CFunDef = this match {
      case CFunV(n) => n
    }
  }
  case class NumV(n: Long) extends InterpreterValue

  trait Interpretable {
    def interpExpr(e: Expr, nv: List[(String, InterpreterValue)], inputs: ListBuffer[Long], outputs: ListBuffer[Long]): InterpreterValue = e match {
      case Num(i) => NumV(i)
      case Prim(Read(), List()) =>
        NumV(inputs.remove(0))
      case Prim(Minus(), List(e)) =>
        NumV(0L - interpExpr(e, nv, inputs, outputs).num())
      case Prim(Plus(), List(e1, e2)) =>
        NumV(interpExpr(e1, nv, inputs, outputs).num() + interpExpr(e2, nv, inputs, outputs).num())
      case Prim(Minus(), List(e1, e2)) =>
        NumV(interpExpr(e1, nv, inputs, outputs).num() - interpExpr(e2, nv, inputs, outputs).num())
    }
  }

  object Interpreter extends Interpretable {
    def interpLint(p: Lint, inputs: ListBuffer[Long], outputs: ListBuffer[Long]): Long = p match {
      case Program(e) => interpExpr(e, Nil, inputs, outputs).num()
    }
  }
}


/* Chapter 2 - Integers and Variables */

object Lvar {

  import Lint.{PrimOp,Expr,Prim}

  case class Print() extends PrimOp // Not in book!

  case class Var(x: String) extends Expr
  case class Let(x: String, e1: Expr, e2: Expr) extends Expr

  abstract class Lvar
  case class Program(body: Expr) extends Lvar


  trait Parsable
    extends ParseExpr[Expr]
      with Lint.Parsable {
    import Library._

    override def reservedWords: List[String] = super.reservedWords ++ List("let", "print")

    override def parseExpr(s: SExpr): Expr = s match {
      case SList(List(SSym("print"), e)) => Prim(Print(), List(parseExpr(e)))
      case SList(List(SSym("let"), SList(List(SSym(x), e1)), e2)) =>
        Let(x, parseExpr(e1), parseExpr(e2))
      case SSym(x) if (!reservedWords.contains(x)) =>
        Var(x)

      case _ => super.parseExpr(s)
    }
  }

  object Parser extends Parsable {
    import Library._
    import Reader._
    def toLvar(s: String): Lvar = parseLvar(read(s))
    def parseLvar(s: SExpr): Lvar = Program(parseExpr(s))
  }


  trait Interpretable
    extends Lint.Interpretable {
    override def interpExpr(e: Expr, nv: List[(String,InterpreterValue)], inputs: ListBuffer[Long], outputs: ListBuffer[Long]): InterpreterValue = e match {
      case Var(x) =>
        nv.find({ case (y, _) => x == y }).getOrElse(throw new RuntimeException("The interpreter to read variable " + x + " that had no value."))._2
      case Let(x, e, body) =>
        interpExpr(body, (x, interpExpr(e, nv, inputs, outputs)) :: nv, inputs, outputs)
      case Prim(Print(), List(e)) =>
        val i = interpExpr(e, nv, inputs, outputs)
        outputs += i.num()
        i
      case _ => super.interpExpr(e, nv, inputs, outputs)
    }
  }

  object Interpreter extends Interpretable {
    def interpLvar(p: Lvar, inputs: ListBuffer[Long], outputs: ListBuffer[Long]): Long = p match {
      case Program(e) => interpExpr(e, Nil, inputs, outputs).num()
    }
  }
}


// Interface


// Interfaces for Cvar

trait PrintStmt[Stmt] {
  def printStmt(s: Stmt): String
}

object Cvar {

  /*
   atm  ::= int | var
   expr ::= atm | (read) | (- atm) | (+ atm atm) | (- atm atm)
   stmt ::= var = expr;
   tail ::= return expr; | stmt tail
   Cvar ::= (label: tail) ...
   */

  import Lint._

  abstract class Stmt
  case class Assign(x: String, e: Expr) extends Stmt

  abstract class Tail
  case class Return(e: Expr) extends Tail
  case class Seq(s: Stmt, t: Tail) extends Tail

  sealed abstract class Cvar
  case class CProgram(blocks: List[(String, Tail)]) extends Cvar

  trait Interpretable
    extends Lint.Interpretable
      with Lvar.Interpretable {

    def interpStmt(s: Stmt, nv: List[(String, InterpreterValue)], inputs: ListBuffer[Long], outputs: ListBuffer[Long], blocks: List[(String, Tail)]): List[(String, InterpreterValue)] = s match {
      case Assign(x, e) => (x, interpExpr(e, nv, inputs, outputs)) :: nv
    }

    def interpTail(t: Tail, nv: List[(String, InterpreterValue)], inputs: ListBuffer[Long], outputs: ListBuffer[Long], blocks: List[(String, Tail)]): InterpreterValue = t match {
      case Return(e) => interpExpr(e, nv, inputs, outputs)
      case Seq(s, t) => interpTail(t, interpStmt(s, nv, inputs, outputs, blocks), inputs, outputs, blocks)
    }
  }

  object Interpreter extends Interpretable {
    def interpCvar(p: Cvar, inputs: ListBuffer[Long], outputs: ListBuffer[Long]): Long = p match {
      case CProgram(List(("start", t))) => interpTail(t, Nil, inputs, outputs, List()).num()
    }
  }

}


object GenSym {
  private var gen: Int = -1
  def freshInt: Int = { gen = 1 + gen; gen }
  def gensym(x: String) = x + "." + freshInt
  def reset = { gen = 0 }
}


