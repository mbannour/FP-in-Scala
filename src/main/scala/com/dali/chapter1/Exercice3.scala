package com.dali.chapter1

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  /*
  Exercice 3.2
   */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("tail of empty list")
    case Cons(x, xs) => xs
  }

  /*
  Exercice 3.2
   */
  def setHead[A](as: List[A], value: A): List[A] = as match {
    case Nil => sys.error("empty list")
    case Cons(x, xs) => Cons(value, xs)
  }

  /*
  Exercice 3.3
   */

  def drop[A](as: List[A], n: Int): List[A] =
    if (n <= 0) as
    else {
      as match {
        case Nil => Nil
        case Cons(x, xs) if n > 0 => drop(xs, n - 1)
      }
    }

  /*
  Exercice 3.3
   */

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case Cons(_, _) => l
  }

  /*
 Exercice 3.4
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error(" init of empty list")
    case Cons(x, xs) =>
      if (xs == Nil) Nil
      else
        Cons(x, init(xs))
  }

}
