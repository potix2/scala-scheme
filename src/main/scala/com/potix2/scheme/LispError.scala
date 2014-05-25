package com.potix2.scheme

abstract class LispError extends Throwable {
  override def toString(): String = this match {
    case UnboundVar(message, varName) => s"${message}: ${varName}";
    case BadSpecialForm(message, form) => s"${message}: ${form}"
    case NotFunction(message, func) => s"${message}: ${func}"
    case NumArgs(expected, found) => s"Expected ${expected} args; found values ${found}"
    case TypeMismatch(expected, found) => s"Invalid type: expected ${expected}, found ${found}"
    case ParseFailure(message) => s"Parse error: ${message}"
  }
}

case class NumArgs(expected: Int, found: List[LispVal]) extends LispError
case class TypeMismatch(expected: String, found: LispVal) extends LispError
case class ParseFailure(message: String) extends LispError
case class BadSpecialForm(message: String, form: LispVal) extends LispError
case class NotFunction(message: String, func: String) extends LispError
case class UnboundVar(message: String, varName: String) extends LispError
case class Default(message: String) extends LispError
