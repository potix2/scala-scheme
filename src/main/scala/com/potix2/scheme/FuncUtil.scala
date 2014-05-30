package com.potix2.scheme

object FuncUtil {
  def flip[A,B,C](f: A => B => C) = { b:B => { a:A => f(a)(b) }}
}
