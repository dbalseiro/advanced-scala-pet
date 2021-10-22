package exercises

case class Email(username: String, host: String)
case class User(name: String, age: Int, email: Email)

object Email:
  def fromString(email: String): Email =
    val rg = "(.*)@(.*)".r
    email match {
      case rg(u, h) => Email(u, h)
      case _ => throw new RuntimeException(s"Invalid Email: $email")
    }

trait Eq[T]:
  def equal(a: T, b: T): Boolean

object Eq:
  def apply[T](a: T, b: T)(implicit instance: Eq[T]) = instance.equal(a, b)

implicit object EmailEq extends Eq[Email]:
  override def equal(a: Email, b: Email): Boolean =
    a.username == b.username && a.host == b.host

implicit object UserEq extends Eq[User]:
  override def equal(a: User, b: User): Boolean =
    a.name == b.name && a.age == b.age && Eq(a.email, b.email)

object TypeClasses extends App:
  val user1 = User("Diego", 22, Email.fromString("dbalseiro@gmail.com"))
  val user2 = User("Diego", 22, Email.fromString("dbalseiro@gmail.com"))
  val user3 = User("Diego", 22, Email.fromString("dbalseiro@stackbuilders.com"))

  println(if Eq(user1, user2) then "1 2 iguales" else "1 2 distintos")
  println(if Eq(user2, user3) then "1 3 iguales" else "1 3 distintos")
