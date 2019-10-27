package sandbox

import cats.Eval
import PrintableInstances._
import PrintableSyntax._

object EvalMonad {

  implicit def printableEval[A] = new Printable[Eval[A]] {
    def format(a: Eval[A]): String = a.toString
  }

  def main(args: Array[String]): Unit = {
    val now = Eval.now(math.random() + 1000)
    val later = Eval.later(math.random() + 2000)
    val always = Eval.always(math.random() + 3000)

    now.print
    later.print
    always.print

    now.value.print
    later.value.print
    always.value.print

    println()

    now.value.print
    later.value.print
    always.value.print

    val greeting = Eval
      .always { println("step 1"); "hello" }
      .map { x => println("step 2"); s"$x, world" }
      .memoize
      .map { x => println("step 3"); s"$x, I'm a cat"}
    
    greeting.print

    greeting.value.print
    greeting.value.print


    // trampolining

    def factorial(n: BigInt): BigInt = if (n == 1) 1 else n * factorial(n - 1)
    def factorialSafe(n: BigInt): Eval[BigInt] = if (n == 1) Eval.now(1) else Eval.defer(factorialSafe(n - 1).map(_ * n))

    // factorial(1_000_000).print
    factorialSafe(100_000).value.print


  }
}