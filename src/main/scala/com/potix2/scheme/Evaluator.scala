package com.potix2.scheme
import scala.reflect.{ClassTag, classTag}

import com.potix2.scheme.LispError.{IOThrowsError, ThrowsError}
import scalaz._
import scalaz.effect._
import scalaz.Scalaz._
import com.potix2.scheme.LispEnv.Env

trait Evaluator {

  val primitives = List(
    ("+", numericBinop (_ + _)),
    ("-", numericBinop (_ - _)),
    ("*", numericBinop (_ * _)),
    //("/", numericBinop (_ / _)),
    ("mod", numericBinop (_ % _)),

    ("=", numBoolBinop (_ == _)),
    ("<", numBoolBinop (_ < _)),
    (">", numBoolBinop (_ > _)),
    ("/=", numBoolBinop (_ != _)),
    (">=", numBoolBinop (_ >= _)),
    ("<=", numBoolBinop (_ <= _)),

    ("&&", boolBoolBinop (_ && _)),
    ("||", boolBoolBinop (_ || _)),

    ("string=?", strBoolBinop (_ == _)),
    ("string<?", strBoolBinop (_ < _)),
    ("string>?", strBoolBinop (_ > _)),
    ("string<=?", strBoolBinop (_ <= _)),
    ("string>=?", strBoolBinop (_ >= _)),

    // conditional expression
    ("if", ifExpr _),

    // list primitives
    ("car", carExpr _),
    ("cdr", cdrExpr _),
    ("cons", consExpr _),

    // predicate for equality
    ("eq?", eqvExpr _),
    ("eqv?", eqvExpr _),
    ("equal?", equalExpr _),

    // type primitives
    ("boolean?", typeTestOp[LispBool]),
    ("string?", typeTestOp[LispString]),
    ("symbol?", typeTestOp[LispAtom]),
    ("number?", typeTestOp[LispInteger])
  )

  def typeTestOp[A <: LispVal: ClassTag]: List[LispVal] => ThrowsError[LispVal] = {
    case xs@(h :: Nil) => LispBool(classTag[A].runtimeClass.isInstance(h)).point[ThrowsError]
    case xs => NumArgs(1,xs).left[LispVal]
  }

  def numericBinop(op: (Int, Int) => Int): List[LispVal] => ThrowsError[LispVal] = {
    case xs@(h :: Nil) => NumArgs(2, xs).left[LispVal]
    case xs => for {
      ints <- xs.map(unpackNumber).sequence[ThrowsError, Int]
    } yield LispInteger(ints.tail.foldLeft(ints.head)(op))
  }

  def numBoolBinop = boolBinop(unpackNumber)(_)
  def strBoolBinop = boolBinop(unpackString)(_)
  def boolBoolBinop = boolBinop(unpackBool)(_)

  def boolBinop[A](unpacker: LispVal => ThrowsError[A])(op: (A,A) => Boolean): List[LispVal] => ThrowsError[LispVal] = {
    case xs@(lhs :: rhs :: Nil) => for {
      l <- unpacker(lhs)
      r <- unpacker(rhs)
    } yield LispBool(op(l, r))
    case xs => NumArgs(2, xs).left[LispVal]
  }

  def unpackNumber(value: LispVal): ThrowsError[Int] = value match {
    case LispInteger(n) => n.point[ThrowsError]
    case v => TypeMismatch("number", v).left[Int]
  }

  def unpackString(value: LispVal): ThrowsError[String] = value match {
    case LispString(v) => v.point[ThrowsError]
    case LispInteger(v) => v.toString.point[ThrowsError]
    case LispBool(v) => v.toString.point[ThrowsError]
    case v => TypeMismatch("string", v).left[String]
  }

  def unpackBool(value: LispVal): ThrowsError[Boolean] = value match {
    case LispBool(b) => b.point[ThrowsError]
    case v => TypeMismatch("boolean", v).left[Boolean]
  }

  def ifExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case xs@(pred :: coseq :: alt :: Nil) => for {
      cond <- eval(pred)
      result <- cond match {
        case LispBool(false) => eval(alt)
        case _ => eval(coseq)
      }
    } yield result
    case xs => NumArgs(3, xs).left[LispVal]
  }

  def carExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case xs@(h::Nil) => h match {
      case xs@(LispList(y :: ys)) => y.point[ThrowsError]
      case xs@(LispDottedList(y :: ys, value)) => y.point[ThrowsError]
      case badArg => TypeMismatch("pair", badArg).left[LispVal]
    }
    case xs@(h::t) => NumArgs(1, xs).left[LispVal]
  }

  def cdrExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case xs@(h::Nil) => h match {
      case xs@(LispList(y :: ys)) => LispList(ys).point[ThrowsError]
      case xs@(LispDottedList(y :: Nil, value)) => value.point[ThrowsError]
      case xs@(LispDottedList(y :: ys, value)) => LispDottedList(ys, value).point[ThrowsError]
      case badArg => TypeMismatch("pair", badArg).left[LispVal]
    }
    case xs@(h::t) => NumArgs(1, xs).left[LispVal]
  }

  def consExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case h::Nil                            => LispList(List(h)).point[ThrowsError]
    case x::LispDottedList(ys,yslast)::Nil => LispDottedList(x::ys, yslast).point[ThrowsError]
    case h::LispList(ys)::Nil              => LispList(h::ys).point[ThrowsError]
    case h::t::Nil                         => LispDottedList(List(h), t).point[ThrowsError]
    case xs@(h::t) => NumArgs(2, xs).left[LispVal]
  }

  def eqvExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case LispBool(b1)::LispBool(b2)::Nil => LispBool(b1 == b2).point[ThrowsError]
    case LispInteger(n1)::LispInteger(n2)::Nil => LispBool(n1 == n2).point[ThrowsError]
    case LispString(s1)::LispString(s2)::Nil => LispBool(s1 == s2).point[ThrowsError]
    case LispAtom(a1)::LispAtom(a2)::Nil => LispBool(a1 == a2).point[ThrowsError]
    case LispDottedList(xs1, x1)::LispDottedList(xs2,x2)::Nil => eqvExpr(List(LispList(xs1 ++ List(x1)), LispList(xs2 ++ List(x2))))
    case LispList(xs1)::LispList(xs2)::Nil => LispBool(xs1 zip xs2 forall { s =>
      eqvExpr(List(s._1,s._2)) match {
        case -\/(err) => false
        case \/-(LispBool(v)) => v
      }}).point[ThrowsError]
    case _::_::Nil => LispBool(false).point[ThrowsError]
    case badArgList => NumArgs(2, badArgList).left[LispVal]
  }

  case class AnyUnpacker[A](unpacker: LispVal => ThrowsError[A])
  val unpackers = List(AnyUnpacker(unpackNumber), AnyUnpacker(unpackString), AnyUnpacker(unpackBool))
  def unpackEquals[A](arg1: LispVal, arg2: LispVal)(unpacker: AnyUnpacker[A]): ThrowsError[Boolean] = {
    val b = for {
      unpacked1 <- unpacker.unpacker(arg1)
      unpacked2 <- unpacker.unpacker(arg2)
    } yield unpacked1 == unpacked2
    if (b.isLeft) false.point[ThrowsError] else b
  }

  def equalExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case LispDottedList(xs1, x1)::LispDottedList(xs2,x2)::Nil => equalExpr(List(LispList(xs1 ++ List(x1)), LispList(xs2 ++ List(x2))))
    case LispList(xs1)::LispList(xs2)::Nil => LispBool(xs1 zip xs2 forall { s =>
      equalExpr(List(s._1,s._2)) match {
        case -\/(err) => false
        case \/-(LispBool(v)) => v
      }}).point[ThrowsError]
    case arg1::arg2::Nil => for {
      primitiveEquals <- unpackers.map(unpackEquals(arg1, arg2)(_)).sequence[ThrowsError, Boolean].map(x => x.foldLeft(false)(_ || _))
      eqvEquals <- eqvExpr(args)
    } yield {
      val eqvEqualsInner = eqvEquals match {
        case LispBool(v) => v
        case _ => false
      }
      LispBool(primitiveEquals || eqvEqualsInner)
    }
    case badArgList => NumArgs(2, badArgList).left[LispVal]
  }

  def exprCond(clauses: List[LispVal]): ThrowsError[LispVal] = clauses.toStream.map {
    case LispList(LispAtom("else")::body) => exprCondImpl(LispBool(true), body)
    case LispList(test::LispAtom("=>")::expr::Nil) => exprCondImpl(test, List(LispList(List(expr, test))))
    case LispList(test::body) => exprCondImpl(test, body)
  }.sequence[ThrowsError, Option[LispVal]].map { _.flatten.head }

  def exprCondImpl(test:LispVal, body:List[LispVal]): ThrowsError[Option[LispVal]] = eval(test) match {
    case \/-(v) => if ( v == LispBool(false) )
      None.point[ThrowsError]
    else
      body.map(eval).sequence[ThrowsError, LispVal].map(x => x.lastOption.getOrElse(v).some)
    case -\/(e) => e.left[Option[LispVal]]
  }

  def lispApply(sym: String, args: List[LispVal]): ThrowsError[LispVal] = primitives.
    collectFirst { case (s,f) if s == sym => args.map(eval).sequence.flatMap(f) }
    .getOrElse(NotFunction("Unrecognized primitives function args", sym).left[LispVal])


  def eval(value: LispVal): ThrowsError[LispVal] = value match {
    case LispList(List(LispAtom("quote"), v)) => v.point[ThrowsError]
    case LispList(LispAtom("cond") :: clauses) => exprCond(clauses)
    case LispList(LispAtom(func) :: args) => lispApply(func, args)
    case s:LispString => s.point[ThrowsError]
    case n:LispNumber => n.point[ThrowsError]
    case b:LispBool   => b.point[ThrowsError]
    case a:LispAtom   => a.point[ThrowsError]
    case LispList(Nil) => value.point[ThrowsError]
    case v => BadSpecialForm("Unrecognized special form", v).left[LispVal]
  }
}
