package exercises

enum IntPropEnum:
  case EVEN, SINGLE

object IntProp:
  def unapply(i: Int): Option[IntPropEnum] =
    if i < 10 then Some(IntPropEnum.SINGLE)
    else if i % 2 == 0 then Some(IntPropEnum.EVEN)
    else None

object CustomPatternMatching extends App:
  val n: Int = 4
  val prop = n match
    case IntProp(IntPropEnum.EVEN) => "Even"
    case IntProp(IntPropEnum.SINGLE) => "Single digit"
    case _ => "No property"

  println(prop)


  abstract class MyList[+A]:
    def head: A = ???
    def tail: MyList[A] = ???

  case object Empty extends MyList[Nothing]
  case class Cons[+A] (override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList:
    def unapplySeq[A](l: MyList[A]): Option[Seq[A]] =
      if l == Empty then Some(Seq.empty)
      else unapplySeq(l.tail).map(l.head +: _)

  val myList: MyList[Int] = Cons(2, Cons(2, Cons(3, Empty)))
  val decomposed = myList match
    case MyList(1, 2, _*) => "Starts with 1 and 2"
    case _ => "whatever"

  println(decomposed)

  case class Person(val name: String, val age: Int)
  val bob = Person("Bobby", 42)

  abstract class Wrapper[T]:
    def isEmpty: Boolean
    def get: T

  object PersonWrapper:
    def unapply(person: Person): Wrapper[String] = new Wrapper[String]:
      override def get: String = person.name
      override def isEmpty = false


  val bobmatch = bob match
    case PersonWrapper("Bob") => "Hello, Bob!"
    case PersonWrapper(name) => s"Hello, My name is $name"
    case _ => "WAT"

  println(bobmatch)