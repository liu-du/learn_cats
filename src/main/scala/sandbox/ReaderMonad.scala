package sandbox

import PrintableSyntax._
import PrintableInstances._
import cats.syntax.applicative._
import cats.data.Reader
import cats.Id

object ReaderMonad {

  implicit def printableId[A] = new Printable[Id[A]] {
    def format(a: cats.Id[A]): String = a.toString
  }
  
  def main(args: Array[String]): Unit = {

    val cola = Cat("cola", 1, "black and white")
    val catName = Reader[Cat, String](cat => cat.name)
    catName
      .map(name => s"hello $name")
      .run(cola)
      .print
    
    case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
    )

    val db = Db(
      Map(
        1 -> "cola",
        2 -> "pudding"
      ),
      Map(
        "cola" -> "coco",
        "pudding" -> "dingding"
      )
    )

    type DbReader[A] = Reader[Db, A]

    implicit def printableDbReader[A] = new Printable[DbReader[A]] {
      def format(a: DbReader[A]): String = a.toString
    }

    def findUsername(userId: Int): DbReader[Option[String]] = 
      Reader(_.usernames.get(userId))
    def checkPassword(username: String, password: String): DbReader[Boolean] = 
      Reader(
        _.passwords.get(username) match {
          case None => false
          case Some(pass) => pass == password
        }
      )
    
    def checkLogin(userId: Int, password: String): DbReader[Boolean] = 
      findUsername(userId)
        .flatMap {
          case None => false.pure[DbReader]
          case Some(username) => checkPassword(username, password)
        }

    checkLogin(1, "coco").run(db).print
    checkLogin(2, "coco").run(db).print
    checkLogin(3, "dingding").run(db).print
    checkLogin(4, "dingding").run(db).print
  }
}