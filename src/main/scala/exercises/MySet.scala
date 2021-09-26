package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean):
  override def apply(elem: A): Boolean = contains(elem)
  def withFilter = filter

  def unary_! : MySet[A]

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(that: MySet[A]): MySet[A]

  def map[B](fxn: A => B): MySet[B]
  def flatMap[B](fxn: A => MySet[B]): MySet[B]
  def filter(pred: A => Boolean): MySet[A]
  def foreach(action: A => Unit): Unit

  def -(elem: A): MySet[A]
  def &(that: MySet[A]): MySet[A]
  def --(that: MySet[A]): MySet[A]


class PropertyBasedSet[A] (property: A => Boolean) extends MySet[A]:
  override def unary_! : MySet[A] = new PropertyBasedSet[A](elem => !property(elem))

  override def contains(elem: A): Boolean = property(elem)
  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || x == elem)
  override def ++(that: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => property(x) || that.contains(x))

  override def map[B](fxn: A => B): MySet[B] = notDefined
  override def flatMap[B](fxn: A => MySet[B]): MySet[B] = notDefined
  override def foreach(action: A => Unit): Unit = notDefined

  override def filter(pred: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && pred(x))

  override def -(elem: A): MySet[A] = this.filter(_ != elem)
  override def &(that: MySet[A]): MySet[A] = this filter that
  override def --(that: MySet[A]): MySet[A] = this filter !that

  private def notDefined = throw new RuntimeException("Not Defined")


class Empty[A] extends MySet[A]:
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = new Cons(elem, this)
  override def ++(that: MySet[A]): MySet[A] = that

  override def map[B](fxn: A => B): MySet[B] = new Empty[B]
  override def flatMap[B](fxn: A => MySet[B]): MySet[B] = new Empty[B]
  override def filter(pred: A => Boolean): MySet[A] = this
  override def foreach(action: A => Unit): Unit = ()

  override def -(elem: A): MySet[A] = this
  override def &(that: MySet[A]): MySet[A] = this
  override def --(that: MySet[A]): MySet[A] = this

  override def unary_! = new PropertyBasedSet[A](Function.const(true))

case class Cons[A] (top: A, rest: MySet[A]) extends MySet[A]:
  override def contains(elem: A): Boolean = top == elem || rest(elem)

  override def +(elem: A): MySet[A] =
    if this(elem) then this
    else new Cons(elem, this)

  override def ++(that: MySet[A]): MySet[A] = rest ++ (that + top)
  override def map[B](fxn: A => B): MySet[B] = (rest map fxn) + fxn(top)
  override def flatMap[B](fxn: A => MySet[B]): MySet[B] = (rest flatMap fxn) ++ fxn(top)

  override def filter(pred: A => Boolean): MySet[A] =
    val filtered = rest filter pred
    if pred(top) then filtered + top
    else filtered

  override def foreach(action: A => Unit): Unit = {action(top); rest foreach action}

  override def -(elem: A): MySet[A] = this.filter(_ != elem)
  override def &(that: MySet[A]): MySet[A] = this.filter(that)
  override def --(that: MySet[A]): MySet[A] = this.filter(!that)

  override def unary_! = new PropertyBasedSet[A](x => !this.contains(x))

object MySet:
  def apply[A](values: A*): MySet[A] =
    @tailrec
    def addValues(seq: Seq[A], acc: MySet[A]): MySet[A] =
      if seq.isEmpty then acc
      else addValues(seq.tail, acc + seq.head)

    addValues(values.toSeq, new Empty[A])

object MySetTest extends App:
  val mySet = MySet(1,2,3,3)
  val mySet2 = MySet(3,4,5)
  val mySet3 = mySet ++ mySet2
  (mySet & mySet2).foreach(print)
  print ("\n")
  (mySet -- mySet2).foreach(print)
  print ("\n")
  (mySet2 -- mySet).foreach(print)
  print ("\n")
  (mySet3 - 3).foreach(print)
