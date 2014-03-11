package com.potix2.scheme

import org.specs2.mutable.SpecificationWithJUnit

class EvaluatorSpec extends SpecificationWithJUnit {
  val evaluator = new Evaluator {}

  "eval an atom" should {
    "return the atom" in {
      evaluator.eval(LispAtom("abcd")) must_== LispAtom("abcd")
    }
  }
  "eval a quoted value" should {
    "return the unquoted value" in {
      evaluator.eval(LispList(List(LispAtom("quote"), LispAtom("abcd")))) must_== LispAtom("abcd")
    }
  }
  "eval" should {
    "reduce (+ 1 2 3) to 6" in {
      evaluator.eval(LispList(List(LispAtom("+"), LispInteger(1), LispInteger(2), LispInteger(3)))) must_== LispInteger(6)
    }
    "reduce (- 4 1 2) to 1" in {
      evaluator.eval(LispList(List(LispAtom("-"), LispInteger(4), LispInteger(1), LispInteger(2)))) must_== LispInteger(1)
    }
    "reduce (+ 2 (- 4 1)) to 5" in {
      evaluator.eval(LispList(List(LispAtom("+"), LispInteger(2), LispList(List(LispAtom("-"), LispInteger(4), LispInteger(1)))))) must_== LispInteger(5)
    }
    "reduce (* 2 3) to 6" in {
      evaluator.eval(LispList(List(LispAtom("*"), LispInteger(2), LispInteger(3)))) must_== LispInteger(6)
    }
  }
}
