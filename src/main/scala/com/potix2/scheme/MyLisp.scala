package com.potix2.scheme

object MyLisp extends App with LispParser {
  override def main(args: Array[String]) {
    println(readExpr(args(0)))
  }
}
