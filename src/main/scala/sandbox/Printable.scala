package sandbox


final case class Cat(name: String, age: Double, color: String)
final case class Box[A](value: A)

trait Printable[T] { self =>
  def format(a: T): String
  def print(a: T): Unit = println(format(a))

  // contra map: appending some transformation
  def contramap[U](func: U => T): Printable[U] =
    new Printable[U] {
      def format(a: U): String = self.format(func(a))
    }
}

// interface object
object Printable {
  // summoner 
  def apply[T](implicit printableT: Printable[T]) = printableT

  // interface methods
  def format[T](x: T)(implicit printable: Printable[T]): String = printable.format(x)
  def print[T](x: T)(implicit printable: Printable[T]): Unit = printable.print(x)
}

object PrintableInstances {
  def pure[T](fn: T => String) = new Printable[T] {
    def format(a: T): String = fn(a)
  }
  implicit val printableString = pure[String](x => x.toString)
  implicit val printableInt = pure[Int](x => x.toString)
  implicit val printableDouble = pure[Double](x => x.toString)
  implicit val printableBoolean = pure[Boolean](if (_) "yes" else "no")
  implicit val printableCat = new Printable[Cat] {
    def format(cat: Cat): String = s"${cat.name} is a ${String.format("%.1f", cat.age)} year-old ${cat.color} cat."
  }
  implicit def printableBox[A: Printable]: Printable[Box[A]] = Printable[A].contramap(_.value)
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

  // interface method
  Printable.print(cola)

  // extension method
  pudding.print

  // contramap prepends transformations
  Printable[Cat]
    .contramap[(String, Double, String)] {
      case (name, age, color) => Cat(name, age, color)
    }.contramap[Double] {
      x => ("new cat", x, "unkown")
    }.print(2)
  
  "fs".print
  true.print

  Printable[Box[Boolean]].print(Box(true))
  
}