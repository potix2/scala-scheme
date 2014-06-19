package com.potix2.scheme

import scalaz.effect.{IO, IORef}
import Lisp._
import java.io.{BufferedWriter, BufferedReader}

sealed trait LispVal {
  def toIORef:IORef[LispVal] = IO.newIORef(this).unsafePerformIO()

  def unwords(xs: List[String]): String =
    xs.foldLeft("")((b,a) => if(b.isEmpty) a else b + " " + a)

  override def toString: String = this match {
    case LispAtom(v)     => v
    case LispList(xs)    => "(" + unwords(xs.map(_.toString)) + ")"
    case LispDottedList(head, tail)    => "(" + unwords(head.map(_.toString)) + " . " + tail.toString() + ")"
    case LispLong(i)     => i.toString()
    case LispDouble(r)   => r.toString
    case LispString(s)   => "\"" + s + "\""
    case LispBool(true)  => "#t"
    case LispBool(false) => "#f"
    case LispChar(c)     => c
    case LispVector(xs)  => "#(" + xs.foldLeft("")((b,a) => a.toString() + " " + b) + ")"
    case LispPrimitiveFunc(_) => "<primitive>"
    case LispFunc(args, varargs, body, env) => "(lambda (" + unwords(args) + varargs.map(" . " + _).getOrElse("") + ") ...)"
    case _ : LispReaderPort => "<IO port>"
    case _ : LispWriterPort => "<IO port>"
    case _ : LispIOFunc => "<IO primitive>"
  }
}

case class LispAtom(value: String) extends LispVal
abstract trait LispPair extends LispVal {
  def isEmpty: Boolean
}

case class LispList(value: List[LispVal]) extends LispPair {
  override def isEmpty: Boolean = value.isEmpty
}

case class LispDottedList(list: List[LispVal], value: LispVal) extends LispPair {
  override def isEmpty: Boolean = false
}

abstract trait LispNumber extends LispVal
case class LispLong(value: Long) extends LispNumber
case class LispDouble(value: Double) extends LispNumber
case class LispString(value: String) extends LispVal
case class LispBool(value: Boolean) extends LispVal
case class LispChar(value: String) extends LispVal
case class LispVector(value: Vector[LispVal]) extends LispVal
case class LispPrimitiveFunc(value: List[LispVal] => ThrowsError[LispVal]) extends LispVal
case class LispFunc(params: List[String], vararg: Option[String], body: List[LispVal], closure: Env) extends LispVal
case class LispIOFunc(params: List[LispVal] => IOThrowsError[LispVal]) extends LispVal
case class LispReaderPort(reader: BufferedReader) extends LispVal {
  def close = reader.close()
}
case class LispWriterPort(writer: BufferedWriter) extends LispVal {
  def close = writer.close()
}

sealed trait Radix
case class Binary extends Radix
case class Oct extends Radix
case class Digit extends Radix
case class Hex extends Radix

