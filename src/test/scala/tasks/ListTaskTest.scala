package tasks

import org.junit.Test
import org.junit.Assert.*
import u03.Lists.List.*
import u02.Optionals.*

class ListTaskTest {

  import Option.{Some, None}

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  val tail = Cons(40, Nil())
  val flatLst = Cons(11, Cons(21, Cons(31, Nil())))
  val flat2Lst = Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil()))))))
  val maxLst = Cons(10, Cons(25, Cons(20, Nil())))

  @Test def dropTest(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1))
    assertEquals(Cons(30, Nil()), drop(lst, 2))
    assertEquals(Nil(), drop(lst, 5))

  /* Da fixare */
  @Test def appendTest(): Unit =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(lst, tail))

  /* Fallisce per l'append */
  @Test def flatMapTest(): Unit =
    assertEquals(flatLst, flatMap(lst)(v => Cons(v + 1, Nil())))
    assertEquals(flat2Lst, flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  /* Fallisce per l'append */
  @Test def mapTest(): Unit =
    assertEquals(flatLst, map(lst)(_ + 1))

  /* Fallisce per l'append */
  @Test def filterTest(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(lst)(_ >= 20))

  @Test def testMax(): Unit =
    assertEquals(Some(25), max(maxLst))
    assertEquals(None(), max(Nil()))

}
