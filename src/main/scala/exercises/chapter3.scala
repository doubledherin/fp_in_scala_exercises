package exercises

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ints: List[Int]): Int = {
    ints match {
      case Nil => 1
      case Cons(0, _) => 0
      case Cons(x, xs) => x * product(xs)
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
      case Nil => Nil
      case Cons(_, xs) => Cons(a, xs)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else {
      l match {
        case Nil => Nil
        case Cons(_, xs) if n > 0 => drop(xs, n-1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println("you're in")
  }
}