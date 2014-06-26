package com.potix2.scheme

import scalaz._
import Scalaz._

/**
 * primitives for list
 */
trait ListPrimitives { self: LispEnv =>
  def exprCar(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case xs@(h::Nil) => h match {
      case xs@(LispList(y :: ys)) => y.point[ThrowsError]
      case xs@(LispDottedList(y :: ys, value)) => y.point[ThrowsError]
      case badArg => TypeMismatch("pair", badArg).left[LispVal]
    }
    case xs@(h::t) => NumArgs(1, xs).left[LispVal]
  }

  def exprCdr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case xs@(h::Nil) => h match {
      case xs@(LispList(y :: ys)) => LispList(ys).point[ThrowsError]
      case xs@(LispDottedList(y :: Nil, value)) => value.point[ThrowsError]
      case xs@(LispDottedList(y :: ys, value)) => LispDottedList(ys, value).point[ThrowsError]
      case badArg => TypeMismatch("pair", badArg).left[LispVal]
    }
    case xs@(h::t) => NumArgs(1, xs).left[LispVal]
  }

  def exprCons(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case h::Nil                            => LispList(List(h)).point[ThrowsError]
    case x::LispDottedList(ys,yslast)::Nil => LispDottedList(x::ys, yslast).point[ThrowsError]
    case h::LispList(ys)::Nil              => LispList(h::ys).point[ThrowsError]
    case h::t::Nil                         => LispDottedList(List(h), t).point[ThrowsError]
    case xs@(h::t) => NumArgs(2, xs).left[LispVal]
  }
}

