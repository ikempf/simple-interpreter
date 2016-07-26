package com.ikempf.lexer

sealed abstract class Expr
case class Addition(lhs: Int, rhs: Int) extends Expr {
  def eval: Int = lhs + rhs
}
case class Substraction(lhs: Int, rhs: Int) extends Expr {
  def eval: Int = lhs - rhs
}

sealed class Token
case class Integer(value: Int) extends Token
case object Plus extends Token
case object Minus extends Token
case object Eof extends Token