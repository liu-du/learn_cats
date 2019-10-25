package sandbox


final case class Cat(name: String, age: Double, color: String)

trait Printable[T] {
  def format(a: T): String
}

// interface method
object Printable {
  def format[T](x: T)(implicit printable: Printable[T]): String = printable.format(x)
  def print[T](x: T)(implicit printable: Printable[T]): Unit = println(printable.format(x))
}

object PrintableInstances {
  def pure[T](fn: T => String) = new Printable[T] {
    def format(a: T): String = fn(a)
  }
  implicit val printableString = pure[String](x => x.toString)
  implicit val printableInt = pure[Int](x => x.toString)
  implicit val printableDouble = pure[Double](x => x.toString)
  implicit val printableCat = new Printable[Cat] {
    def format(cat: Cat): String = s"${cat.name} is a ${String.format("%.1f", cat.age)} year-old ${cat.color} cat."
  }
}

// extension method (aka syntax)
object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {
    def format(implicit printable: Printable[A]): String = printable.format(a)
    def print(implicit printable: Printable[A]): Unit = println(format)
  }
}

object PrintableMain extends App {

  import PrintableInstances._
  import PrintableSyntax._

  val cola = Cat("coco", 11d / 12d, "black and white")
  val pudding = Cat("ding ding", 7d / 12d, "brown")

  Printable.print(cola)
  pudding.print
}