package sandbox

import cats.data.State

import PrintableInstances._
import PrintableSyntax._
import cats.Eval
import cats.data.IndexedState
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object StateMonad {

  implicit def printableEval2[A] = new Printable[Eval[A]] {
    def format(a: Eval[A]): String = a.value.toString()
  }

  implicit def printableEval[A, B] = new Printable[Eval[(A, B)]] {
    def format(a: Eval[(A,B)]): String = a.value.toString()
  }

  implicit def printableState[A, B] = new Printable[State[A, B]] {
    def format(a: cats.data.State[A,B]): String = a.toString()
  }

  def main(args: Array[String]): Unit = {
    val res1 = State[Int, String] {
      state => (state, s"the state is $state")
    }
    .flatMap(x => State(state => (state + 5, s"$x; the state was $state")))
    .flatMap(x => State(state => (state + 5, s"$x; the state was $state")))
    .map(x => x + "!")

    res1.print
    res1.run(5).print

    val res2 = for {
      x <- State[Int, String](state => (state, s"the state is $state"))
      y <- State[Int, String](state => (state + 5, s"$x; the state was $state"))
      z <- State[Int, String](state => (state + 5, s"$y; the state was $state"))
    } yield z + "!"

    res2.run(5).print

    // ** primitive state monad constructors **

    // extract state
    State.get[Int].run(5).print

    // use map to sequence computation that doesn't depend on state
    State.get[Int].map(_ * 2).run(5).print


    // ignore previouse state by setting it to some value and set result to Unit
    State.set(10).run(5).print

    // lift a plain value into State
    State.pure[Int, String]("hello").run(5).print

    // inspect without modifying state
    State.inspect[Int, String](state => s"the state is $state").run(5).print

    // modify the state and discard result
    State.modify[Int](_ + 1).run(5).print

    // use modify to sequence computation that mutate state but doesn't affect result
    State.get[Int].modify(_ * 2).run(5).print

    import State._

    val res = for {
      a <- get[Int]
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 100)
      e <- apply[Int, Int](s => (s + 10, c + s))
    } yield (a, b, c, e)

    res.run(5).print
    println("======================")

    type CalcState[A] = State[List[Int], A]

    def op(func: (Int, Int) => Int): CalcState[Try[Int]] = 
      State[List[Int], Try[Int]] {
        case a :: b :: tail => 
          val ans = func(b, a)
          (ans :: tail, Success(ans))
        case stack => (stack, Failure(new Exception("Inconsisent stack.")))
      }

    def evalOne(sym: String): CalcState[Try[Int]] = sym match {
      case s if s.matches("^\\d+$") => apply( stack => (sym.toInt :: stack, Success(sym.toInt)) )
      case "*" => op(_ * _)
      case "+" => op(_ + _)
      case "/" => op(_ / _)
      case "-" => op(_ - _)
      case op => State.pure(Failure(new NotImplementedError(s"operation $op is not implemented.")))
    }
    
    val program = for {
      _ <- evalOne("5")
      _ <- evalOne("7")
      ans <- evalOne("*")
    } yield ans

    program.run(Nil).print

    println("======================")

    def evalAll(input: List[String]): CalcState[Try[Int]] = 
      input
        .foldLeft[CalcState[Try[Int]]](
          State.pure(Failure(new Exception("Initial state")))
        )((m, sym) => m.flatMap(_ => evalOne(sym)))


    evalAll(List("1", "2", "+", "4", "*", "10", "20", "+", "*", "%")).run(Nil).print

    val result = for {
      _ <- evalAll(List("1", "9", "+", "6", "3", "/", "/"))
      _ <- evalAll(List("1"))
      resC <- evalAll(List("2", "*", "*"))
    } yield resC

    result.run(Nil).print

    def evalInput(s: String): CalcState[Try[Int]] = 
      evalAll(s.split(" ").toList)
    
    evalInput("1 2 3 4 5 * * * +").run(Nil).print
  }
}