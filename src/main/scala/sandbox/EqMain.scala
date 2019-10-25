package sandbox

import cats.Eq
import cats.instances.all._
import cats.syntax.eq._
import cats.syntax.option._

object EqMain {
  def main(args: Array[String]): Unit = {

    val eqInt = Eq[Int]


    // Here we're comparing Option[Int] with Option[String], the 
    // the code compiles, but this shouldn't compile in most cases, because they're obviously not equal
    List(1,2,3).map(Option(_)).filter(_ == Some("abc")).foreach(println)

    // use Eq type class, the following can't compile
    // List(1,2,3).map(Option(_)).filter(x => x === 1).foreach(println)

    // scala actually produces a compile time error if comparing two different simply types:
    // println(1 == "abc")

    // implement an instance for custom type
    implicit val eqCat = Eq.instance[Cat] { 
      case (Cat(name1, age1, color1), Cat(name2, age2, color2)) => 
        name1 === name2 && age1 === age2 && color1 === color2
    }

    println(PrintableMain.cola === PrintableMain.pudding)
    println(PrintableMain.cola === PrintableMain.cola)
  }

}
