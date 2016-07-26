package com.ikempf.lexer

import cats.data.Validated.Valid
import org.scalatest.{FlatSpec, Matchers}

class LexerTest extends FlatSpec with Matchers {

  "Lexer" should "tokenize given input" in {
    // Given
    val tokens = (for {
      t1 <- Lexer.nextToken
      t2 <- Lexer.nextToken
      t3 <- Lexer.nextToken
      t4 <- Lexer.nextToken
      t4 <- Lexer.nextToken
      t5 <- Lexer.nextToken
    } yield List(t1, t2, t3, t4, t5)).runA("5+3").value

    // When / Then
    tokens(0) should equal(Valid(Integer(5)))
    tokens(1) should equal(Valid(Plus))
    tokens(2) should equal(Valid(Integer(3)))
    tokens(3) should equal(Valid(Eof))
    tokens(4) should equal(Valid(Eof))
  }

  it should "tokenize mutiple digit integers" in {
    // Given
    val tokens = Lexer.collectAll.runA("501 + 13").value

    // When / Then
    tokens(0) should equal(Valid(Integer(501)))
    tokens(1) should equal(Valid(Plus))
    tokens(2) should equal(Valid(Integer(13)))
  }

  it should "ignore whitespaces" in {
    // Given
    val tokens = Lexer.collectAll.runA("501 + 13").value

    // When / Then
    tokens(0) should equal(Valid(Integer(501)))
    tokens(1) should equal(Valid(Plus))
    tokens(2) should equal(Valid(Integer(13)))
  }

  it should "evaluate addition" in {
    // Given / When
    val expr = Lexer.nextExpr.runA("101 + 199").value

    // Then
    expr.toOption.get.asInstanceOf[Addition].eval should equal(300)
  }

  it should "evaluate substraction" in {
    // Given / When
    val expr = Lexer.nextExpr.runA("350 - 49").value

    // Then
    expr.toOption.get.asInstanceOf[Substraction].eval should equal(301)
  }

  it should "produce syntax error" in {
    // Given / When
    val expr = Lexer.nextExpr.runA("101 $ 199").value

    // Then
    expr.isInvalid should be(true)
    expr.toEither.left.get should include("Syntax error")
  }
}
