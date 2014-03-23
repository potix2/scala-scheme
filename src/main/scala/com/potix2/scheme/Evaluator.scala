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

  def unpackNumber(value: LispVal): ThrowsError[Int] = value match {
    case LispInteger(n) => n.point[ThrowsError]
    case v => TypeMismatch("number", v).left[Int]
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
    case v => BadSpecialForm("Unrecognized special form", v).left[LispVal]
  }
}
