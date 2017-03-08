package exercises

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(n, ns) => n + sum(ns)
    }
  }

  def product(ints: List[Int]): Int = {
    ints match {
      case Nil => 1
      case Cons(0, _) => 0
      case Cons(n, ns) => n * product(ns)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }
  }

  def setHead[A](a: A, as: List[A]): List[A] = {
    as match {
      case Nil => List(a)
      case Cons(_, _) => Cons(a, as)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n-1)
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case Cons(_, _) => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, xs) if xs == Nil =>  Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def foldLeftHelper(xs: List[A], acc: B): B = {
      xs match {
        case Nil => acc
        case Cons(h, _) => foldLeftHelper(xs, f(acc, h))
      }
    }
    foldLeftHelper(as, z)
  }


  def lengthWithFoldRight[A](as: List[A], z: A)(f: A => Int): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  def sumWithFoldLeft[A](ints: List[Int], z: Int)(f: Int => Int): Int = {
    foldLeft(ints, 0)((acc, _) => acc + 1)
  }

  def main(args: Array[String]): Unit = {
    println("you're in")
  }
}