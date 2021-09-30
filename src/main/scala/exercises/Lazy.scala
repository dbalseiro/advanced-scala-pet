package exercises

trait Lazy [+A]:
  def eval: A
  def flatMap[B](fxn: A => Lazy[B]): Lazy[B]

  def map[B](fxn: A => B): Lazy[B] = this.flatMap(a => Lazy(fxn(a)))

object Lazy:
  def apply[A](value: => A): Lazy[A] = new Eval(value)
  def flatten[A](that: Lazy[Lazy[A]]): Lazy[A] = that.flatMap(la => la)

class Eval[+A] (v: => A) extends Lazy[A]:
  override lazy val eval = v
  override def flatMap[B](fxn: A => Lazy[B]): Lazy[B] = fxn(eval)

object LazyTest extends App:
  val a = Lazy({println("lol"); 42})
  val b = Lazy(a.map(_+1))

  println(Lazy.flatten(b).eval)


  def f (e: Int) = Lazy({println("double"); e*e})

  println("wat")
  println(a.flatMap(f).eval)