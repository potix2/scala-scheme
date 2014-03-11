package com.potix2.scheme

trait Evaluator {
  val primitives = List(
    ("+", numericBinop (_ + _)),
    ("-", numericBinop (_ - _)),
    ("*", numericBinop (_ * _)),
    //("/", numericBinop (_ / _)),
    ("mod", numericBinop (_ % _))
  )

  def numericBinop(op: (Int, Int) => Int): List[LispVal] => LispVal =
    (xs: List[LispVal]) => LispInteger(xs.map(unpackNumber) match {
      case h :: t => t.foldLeft(h)(op)
    })

  def unpackNumber(value: LispVal): Int = value match {
    case LispInteger(n) => n
    case _ => 0
  }

  def lispApply(sym: String, args: List[LispVal]): LispVal = primitives.
      collectFirst { case (s,f) if s == sym => f(args) }.
      getOrElse(LispBool(false))

  def eval(value: LispVal): LispVal = value match {
    case LispList(List(LispAtom("quote"), v)) => v
    case LispList(LispAtom(func) :: args) => lispApply(func, args.map(eval))
    case x => x
  }
}
