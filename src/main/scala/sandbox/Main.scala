package sandbox

import cats.Show
import cats.syntax.show._
import cats.instances.int._
import java.sql.Date

object Main extends App {

  // use default type class instances
  println(123.show)

  // override default type class instances
  implicit val stringShow: Show[String] = Show.show(t => s"a string: ${t}")
  println("abc".show)

  // define a new type class instance for Date, Show[Date]
  implicit val dateShow: Show[Date] = Show.show(t => s"${t.getTime}ms since the epoch")
  println(Date.valueOf("2019-01-01").show)

}
