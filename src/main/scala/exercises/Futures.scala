/**
 * 1) Fulfill a promise IMMEDIATELY with a value
 * 2) inSequence(fa, fb)
 * 3) first(fa, fb) => return the future that finishes first
 * 4) last(fa, fb) => return the future that finishes last
 * 5) retryUntil[T](acton: () => Future[T], condition: T => Boolean): Future[T]
 * */
package exercises

import scala.concurrent.{Future, Promise}
import concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success, Try}

object FuturesExercise:
  def fulfill[T](value: T): Future[T] = Future(value)

  def inSequence[A, B](futureA: Future[A], futureB: Future[B]): Future[B] =
    futureA.flatMap(_ => futureB)

  def first[T](futureA: Future[T], futureB: Future[T]): Future[T] =
    val promise = Promise[T]
    futureA.onComplete(promise.tryComplete)
    futureB.onComplete(promise.tryComplete)
    promise.future

  def last[T](futureA: Future[T], futureB: Future[T]): Future[T] =
    val promiseBoth = Promise[T]
    val promiseLast = Promise[T]

    def checkResult(result: Try[T]): Unit =
      if !promiseBoth.tryComplete(result)
      then promiseLast.complete(result)

    futureA.onComplete(checkResult)
    futureB.onComplete(checkResult)
    promiseLast.future

  def retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T] =
    action().filter(condition).recoverWith({
      case _ => retryUntil(action, condition)
    })


object FuturesTest extends App:
  val future1 = Future { Thread.sleep(1000); print("A"); 41 }
  val futureHalf = Future { Thread.sleep(500); print("B"); 43 }

  FuturesExercise.last(future1, futureHalf).onComplete(println)

  Thread.sleep(2000)

  def randomFuture = () => Future {
    Thread.sleep(100)
    val v = Random.nextInt(100)
    println(v); v
  }

  FuturesExercise.retryUntil(randomFuture, _ > 50)
  Thread.sleep(2000)
