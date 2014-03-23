package com.potix2.scheme

import scalaz.{\/-, -\/}

object MyLisp extends App with LispParser with Evaluator {

  override def main(args: Array[String]) {
    val extractValue = for {
      expr <- readExpr(args(0))
      evaled <- eval(expr)
    } yield evaled
    extractValue match {
      case -\/(e) => println(e)
      case \/-(v) => println(v)
    }
  }
}
