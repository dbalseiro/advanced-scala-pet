package exercises

implicit class PimpedString(val s: String):
  def asInt: Int = s.toInt
  def encrypt: String =
    def rotate(a: Char): Char = (a.toInt + 2).toChar
    s.toCharArray.map(rotate).mkString

implicit class PimpedInt(val i: Int):
  def times(f: Int => Unit): Unit = (1 to i).map(f)
  def *[T] (l: List[T]): List[T] = (1 to i).toList.flatMap(_ => l)

object ImpliitClasses extends App:
  println("8".asInt)
  println("JYohnz".encrypt)
  3.times(i => println(i + 3))
  println(2 * List(1, 2, 3))