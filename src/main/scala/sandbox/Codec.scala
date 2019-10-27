package sandbox

trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    new Codec[B] {
      def encode(value: B): String = self.encode(enc(value))
      def decode(value: String): B = dec(self.decode(value))
    }
}

object Codec {

  // summoner
  def apply[A](implicit codec: Codec[A]) = codec

  // constructor
  def instance[A](enc: A => String, dec: String => A) = new Codec[A] {
    def encode(value: A): String = enc(value)
    def decode(value: String): A = dec(value)
  }

  // instances
  implicit val intCodec = instance[Int](
    _.toString, 
    _.toInt
  )
  implicit val booleanCodec = instance[Boolean](
    if (_) "yes" else "no", 
    (x: String) => x == "yes"
  )

  // syntax
  implicit class codecOps[A : Codec](a: A) {
    def encode: String = Codec[A].encode(a)
  }
}


object CodecMain {

  import Codec._
  import PrintableSyntax._
  import PrintableInstances._

  final case class Cat(age: Int)
  implicit val printableCat = new Printable[Cat] {
    def format(a: Cat): String = a.toString
  }

  def main(args: Array[String]): Unit = {
    def encocde[A](value: A)(implicit c: Codec[A]): String = c.encode(value)  
    def decocde[A](value: String)(implicit c: Codec[A]): A = c.decode(value)  


    true.encode.print
    decocde[Boolean]("no").print

    implicit val catCodec = intCodec.imap[Cat](Cat(_), _.age)

    Cat(4).encode.print

    decocde[Cat]("5").print

  }
}