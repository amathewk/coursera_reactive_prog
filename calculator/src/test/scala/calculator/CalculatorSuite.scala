package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("delta") {
    val a = Signal(3.0)
    val b = Signal(7.0)
    val c = Signal(2.0)
    println (Polynomial.computeDelta(a,b,c)())
    assert(Polynomial.computeDelta(a,b,c)() == 25.0)
  }

  test("compute solution") {
    val a = Signal(1.0)
    val b = Signal(6.0)
    val c = Signal(1.0)
    val d = Signal(32.0)
//    assert(Polynomial.computeSolutions(a,b,c,d)() == Set (-0.1715,-5.8284))
  }

  test("detect cycle in simple mutual reference") {
    val references:Map[String, Signal[Expr]] = Map("a" -> Signal(Ref("b")), "b" -> Signal(Ref("a")))
    assert(Calculator.hasCycle(Ref("b"), references, List("a")))
  }

  test("compute solutions with cycle in simple mutual reference") {
    val c = Calculator.computeValues(Map(
      "a" -> Signal(Ref("b")),
      "b" -> Signal(Ref("a")),
      "c" -> Signal(Literal(3.0)),
      "d" -> Signal(Ref("c"))
    ))
    assert(c("d")() == 3.0)
//    assert(c("a")() == Double.NaN)
//    assert(c("b")() == Double.NaN)
  }

}
