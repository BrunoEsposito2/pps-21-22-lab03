package u03

import u02.Optionals.*

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
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => flatMap(l)(s => Cons(h, filter(t)(pred)))
      case Cons(_, t) => filter(t)(pred) //flatMap(t)(s => Cons(s, filter(t)(pred)))
      case Nil() => Nil()

    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n-1)
      case Cons(h, t) if n == 0 => l
      case Nil() => Nil()

    /* Da fixare */
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Cons(h, t), Cons(h2, t2)) => append(Cons(h, Cons(h2, t)), t2)
      case (Cons(h2, t2), Nil()) => left

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, Cons(h1, t)) if h >= h1 => max(Cons(h, t))
      case Cons(h, Cons(h1, t)) if h1 >= h => max(Cons(h1, t))
      case Cons(h, _) => Some(h)
      case Nil() => None()

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52

  println(drop(l, 1))
  println(drop(l, 2))
  println(drop(l, 5))

  /* Da fixare */
  val tail = Cons(40, Nil())
  println(append(l, tail))

  println(flatMap(l)(v => Cons(v + 1, Nil())))
  println(flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  /**
  * Task part 2 (more on lists)
  */

  /* TODO: Person */