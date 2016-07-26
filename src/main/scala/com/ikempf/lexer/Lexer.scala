package com.ikempf.lexer

import cats.data.Validated.{Invalid, Valid}
import cats.data.{State, Validated}

object Lexer {

  def nextToken: State[String, Validated[String, Token]] =
    State(input => {
      val cleanInput = input.dropWhile(_.isWhitespace)

      if (cleanInput.isEmpty) {
        (cleanInput, Valid(Eof))
      }
      else if (cleanInput.head.isDigit) {
        val int = cleanInput.takeWhile(_.isDigit)
        (cleanInput.drop(int.length), Valid(Integer(int.toInt)))
      }
      else if (cleanInput.head == '+') {
        (cleanInput.drop(1), Valid(Plus))
      }
      else if (cleanInput.head == '-') {
        (cleanInput.drop(1), Valid(Minus))
      }
      else {
        (cleanInput.drop(1), Invalid("Syntax error when reading token in " + cleanInput.take(1)))
      }
    })

  def nextExpr: State[String, Validated[String, Expr]] =
    collectAll.map {
      case Valid(Integer(lhs)) :: Valid(Plus) :: Valid(Integer(rhs)) :: t => Valid(Addition(lhs, rhs))
      case Valid(Integer(lhs)) :: Valid(Minus) :: Valid(Integer(rhs)) :: t => Valid(Substraction(lhs, rhs))
      case l => Invalid("Syntax error when reading expression in " + l)
    }

  def trimLeft(input: String): String =
    input.dropWhile(_.isWhitespace)

  def collectAll: State[String, List[Validated[String, Token]]] = {
    nextToken.flatMap(token => {
      if (token.exists(_ == Eof))
        State((_, token :: Nil))
      else
        collectAll.map(token :: _)
    })
  }
}