package u03

import u02.Optionals.*
import u03.Lists.List.foldLeft

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    import Option.{Some, None}

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    /**
     * Task Part 1 (lists)
     */
    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => flatMap(l)(h => Cons(mapper(h), Nil()))
      case _ => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => flatMap(l1)(s => Cons(h, filter(t)(pred)))
      case Cons(_, Cons(h, t)) => Cons(h, filter(t)(pred))
      case _ => Nil()

    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n-1)
      case _ => l

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case _ => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case _ => Nil()

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, Cons(h1, t)) if h >= h1 => max(Cons(h, t))
      case Cons(h, Cons(h1, t)) if h1 >= h => max(Cons(h1, t))
      case Cons(h, _) => Some(h)
      case _ => None()

    /**
    * Task part 2 es.4
    */
    def foldLeft(lst: List[Int])(reduce: Int)(op: (Int, Int) => Int): Int = lst match
      case Cons(h, t) => foldLeft(t)(op(reduce, h))((reduce, h1) => op(reduce, h1))
      case _ => reduce

    def foldRight(lst: List[Int])(reduce: Int)(op: (Int, Int) => Int): Int = lst match
      case Cons(h, t) => foldRight(t)(op(h, reduce))((h1, reduce) => op(h1, reduce))
      case _ => reduce

  /*
  * Prints of foldLeft and foldRight
  *
  val lst = List.Cons(3, List.Cons(7, List.Cons(1, List.Cons(5, List.Nil()))))
  println(foldLeft(lst)(0)(_ - _))

  val lst = List.Cons(3, List.Cons(7, List.Cons(1, List.Cons(5, List.Nil()))))
  println(List.foldRight(lst)(0)(_ - _))
  */

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52

  println(drop(l, 1))
  println(drop(l, 2))
  println(drop(l, 5))

  val tail = Cons(40, Nil())
  println(append(l, tail))

  println(flatMap(l)(v => Cons(v + 1, Nil())))
  println(flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  /**
  * Task part 2 es.3
  */

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:

    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    def isStudent(p: Person): Boolean = p match
      case Student(_, _) => true
      case _ => false

    def course[Teacher](p: Person): String = p match
      case Teacher(_, c) => c
      case _ => null

    def getCourses(p: List[Person]): List[String] = p match
      case Cons(h, t) => Cons(course(h), map(filter(t)(course(_) == course(h)))(course(_)))
      case Nil() => Nil()
      case _ => Nil()

    def getCoursesFlatmap(p: List[Person]): List[String] = p match
      case Cons(h, t) => Cons(course(h), filter(flatMap(t)(s => Cons(course(s), Nil())))(_ == course(h)))
      case Nil() => Nil()
      case _ => Nil()
