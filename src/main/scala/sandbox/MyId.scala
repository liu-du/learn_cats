package sandbox

import cats.Id

object MyId {
  // trait Id[A] {}

  def pure[A](a: A): Id[A] = a
  def fmap[A, B](ida: Id[A])(fun: A => B): Id[B] = fun(ida)
  def flatMap[A, B](ida: Id[A])(fun: A => Id[B]): Id[B] = fun(ida)

  def main(args: Array[String]): Unit = {
    import PrintableInstances._
    import PrintableSyntax._

    fmap(123)(_ * 2).print
  }
}