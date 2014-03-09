package com.potix2.scheme

object MyLisp extends App with LispParser {
  def eval(value: LispVal): LispVal = value match {
    case LispList(List(LispAtom("quote"), v)) => v
    case x => x
  }

  override def main(args: Array[String]) {
    println(eval(readExpr(args(0))))
  }
}
