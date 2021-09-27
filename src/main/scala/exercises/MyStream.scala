package exercises

import exercises.EmptyStream.NonEmptyStream

abstract class MyStream[+A]:
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A] (elem: B): MyStream[B]
  def ++[B >: A]  (that: => MyStream[B]): MyStream[B]

  def foreach(action: A => Unit): Unit
  def map[B](fxn: A => B): MyStream[B]
  def flatMap[B](fxn: A => MyStream[B]): MyStream[B]
  def filter(pred: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A]
  def drop(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A]

  def zip[B, C](zipper: (A, B) => C, that: MyStream[B]): MyStream[C]


object MyStream:
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new NonEmptyStream[A](start, from(generator(start))(generator))


object EmptyStream extends MyStream[Nothing]:
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("head: Invalid operation on empty streams")
  override def tail: MyStream[Nothing] = throw new NoSuchElementException("head: Invalid operation on empty streams")

  override def #::[B >: Nothing] (elem: B): MyStream[B] = new NonEmptyStream[B](elem, this)
  override def ++[B >: Nothing]  (that: => MyStream[B]): MyStream[B] = that

  override def foreach(action: Nothing => Unit): Unit = ()
  override def map[B](fxn: Nothing => B): MyStream[B] = this
  override def flatMap[B](fxn: Nothing => MyStream[B]): MyStream[B] = this
  override def filter(pred: Nothing => Boolean): MyStream[Nothing] = this

  override def take(n: Int): MyStream[Nothing] = this
  override def drop(n: Int): MyStream[Nothing] = this
  override def takeAsList(n: Int): List[Nothing] = Nil

  override def zip[B, C](zipper: (Nothing, B) => C, that: MyStream[B]): MyStream[C] = this

  class NonEmptyStream[+A](override val head: A, tl: => MyStream[A]) extends MyStream[A]:
    override lazy val tail: MyStream[A] = tl

    override def isEmpty: Boolean = false
    override def #::[B >: A] (elem: B): MyStream[B] = new NonEmptyStream[B](elem, this)
    override def ++[B >: A]  (that: => MyStream[B]): MyStream[B] = new NonEmptyStream[B](head, tail ++ that)

    override def foreach(action: A => Unit): Unit = {action(head); tail.foreach(action)}
    override def map[B](fxn: A => B): MyStream[B] = new NonEmptyStream[B](fxn(head), tail.map(fxn))
    override def flatMap[B](fxn: A => MyStream[B]): MyStream[B] = fxn(head) ++ tail.flatMap(fxn)
    override def filter(pred: A => Boolean): MyStream[A] =
      lazy val filtered = tail.filter(pred)
      if pred(head) then new NonEmptyStream[A](head, filtered)
      else filtered

    override def drop(n: Int): MyStream[A] =
      if n == 0 then this
      else tail.drop(n-1)

    override def take(n: Int): MyStream[A] =
      if n == 0 then EmptyStream
      else new NonEmptyStream[A](head, tail.take(n-1))

    override def takeAsList(n: Int): List[A] =
      if n == 0 then Nil
      else head :: tail.takeAsList(n-1)

    override def zip[B, C](zipper: (A, B) => C, that: MyStream[B]): MyStream[C] =
      zipper(this.head, that.head) #:: this.tail.zip(zipper, that.tail)

object MyStreamTest extends App:
  val naturals = MyStream.from(1)(_+1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val from0 = 0 #:: naturals
  println(from0.head)

  from0.take(10000).foreach(println)

  println(from0.map(_*2).takeAsList(10))
  println(from0.flatMap(x => new NonEmptyStream(x+2, EmptyStream)).takeAsList(10))

  println(from0.filter(_<10).take(9).takeAsList(11))

  def fibos(first: BigInt, second: BigInt): MyStream[BigInt] =
    new NonEmptyStream[BigInt](first, fibos(second, first + second))

  val fibonacci = fibos(1,1)

  println(fibonacci.takeAsList(100))
  println(fibonacci.drop(10).head)

  val from2 = naturals.drop(1)
  def sieve(seed: MyStream[Int]): MyStream[Int] =
    val hd = seed.head
    val tl = seed.tail.filter(_ % hd != 0)
    new NonEmptyStream[Int](hd, sieve(tl))

  val erathostenes = sieve(from2)
  println(erathostenes.takeAsList(100))