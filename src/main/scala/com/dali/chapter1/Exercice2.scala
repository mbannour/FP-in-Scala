package com.dali.chapter1

import scala.annotation.tailrec

object Exercice2 {

  /*
  Exercice2.1  nth Fibonacci
   */

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int, acc1: Int): Int = {
      if (n == 0) acc
      else
        go(n - 1, acc1, acc + acc1)
    }
    go(n, 0, 1)
  }

  /*
   Exercice2.2
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n == as.length - 1)
        true
      else if (ordered(as(n), as(n + 1)))
        false
      else
        loop(n + 1)

    }
    loop(0)
  }

  /*
   Exercice2.3
   */

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  { a: A => b: B =>
    f(a, b)
  }

  /*
  Exercice2.4
   */

  def compose[A, B, C](f: B => C, g: A => B): A => C = { x : A =>
    f(g(x))
  }

}
