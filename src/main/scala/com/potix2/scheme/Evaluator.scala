package com.potix2.scheme
import scala.reflect.{ClassTag, classTag}

import com.potix2.scheme.LispError.ThrowsError
import scalaz.Scalaz._

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

  def lispApply(sym: String, args: List[LispVal]): ThrowsError[LispVal] = primitives.
      collectFirst { case (s,f) if s == sym => f(args) }.
      getOrElse(NotFunction("Unrecognized primitives function args", sym).left[LispVal])

  def eval(value: LispVal): ThrowsError[LispVal] = value match {
    case LispList(List(LispAtom("quote"), v)) => v.point[ThrowsError]
    case LispList(LispAtom(func) :: args) =>
      val argList:ThrowsError[List[LispVal]] = args.map(eval).sequence[ThrowsError, LispVal]
      for {
        a <- argList
        result <- lispApply(func, a)
      } yield result
    case s:LispString => s.point[ThrowsError]
    case n:LispNumber => n.point[ThrowsError]
    case b:LispBool   => b.point[ThrowsError]
    case a:LispAtom   => a.point[ThrowsError]
    case v => BadSpecialForm("Unrecognized special form", v).left[LispVal]
  }
}
