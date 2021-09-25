package playground

import scala.annotation.tailrec

object Playground extends App:
  def factorial(n: BigInt): BigInt =
    @tailrec
    def memo(acc: BigInt, i: BigInt): BigInt =
      if i == n + 1 then acc
      else memo(i * acc, i+1)
    memo(1, 1)

  println(factorial(5000))
