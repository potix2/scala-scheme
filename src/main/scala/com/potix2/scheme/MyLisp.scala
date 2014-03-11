package com.potix2.scheme

object MyLisp extends App with LispParser with Evaluator {

  override def main(args: Array[String]) {
    println(eval(readExpr(args(0))))
  }
}
