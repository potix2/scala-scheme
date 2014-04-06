package com.potix2.scheme

sealed trait LispVal {
  override def toString: String = this match {
    case LispAtom(v)     => v
    case LispList(xs)    => "(" + xs.foldLeft("")((b,a) => if(b.isEmpty) a.toString() else b + " " + a.toString()) + ")"
    case LispDottedList(head, tail)    => "(" + head.foldLeft("")((b,a) => a.toString() + " " + b) + ". " + tail.toString() + ")"
    case LispInteger(i)  => i.toString()
    case LispString(s)   => "\"" + s + "\""
    case LispBool(true)  => "#t"
    case LispBool(false) => "#f"
    case LispChar(c)     => c
    case LispVector(xs)  => "#(" + xs.foldLeft("")((b,a) => a.toString() + " " + b) + ")"
  }
}

case class LispAtom(value: String) extends LispVal
case class LispList(value: List[LispVal]) extends LispVal
case class LispDottedList(list: List[LispVal], value: LispVal) extends LispVal
abstract trait LispNumber extends LispVal
//case class LispComplex(r: Double, i: Double) extends LispNumber
//case class LispReal(value: Double) extends LispNumber
//case class LispUReal(value: Int) extends LispNumber
//case class LispDecimal(value: Int) extends LispNumber
case class LispInteger(value: Int) extends LispNumber
case class LispString(value: String) extends LispVal
case class LispBool(value: Boolean) extends LispVal
case class LispChar(value: String) extends LispVal
case class LispVector(value: Vector[LispVal]) extends LispVal

sealed trait Radix
case class Binary extends Radix
case class Oct extends Radix
case class Digit extends Radix
case class Hex extends Radix

