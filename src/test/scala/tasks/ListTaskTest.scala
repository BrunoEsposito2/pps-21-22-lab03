package tasks

import org.junit.Test
import org.junit.Assert.*
import u03.Lists.List.*
import u02.Optionals.*
import u03.Lists.Person.*

class ListTaskTest {

  import Option.{Some, None}
  import u03.Lists.Person.{Teacher, Student}

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  val tail = Cons(40, Nil())
  val flatLst = Cons(11, Cons(21, Cons(31, Nil())))
  val flat2Lst = Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil()))))))
  val maxLst = Cons(10, Cons(25, Cons(20, Nil())))
  val foldlst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  val foldOp: (Int, Int) => Int = _ - _

  val t1Name = "mario"
  val t2Name = "giulio"
  val c1 = "matematica"
  val c2 = "inglese"
  val c3 = "italiano"
  val foldReduce = 0

  /**
   * Tests of Task part 1
   */

  @Test def dropTest(): Unit =
    assertEquals(lst, drop(lst, 0))
    assertEquals(Nil(), drop(Nil(), 1))
    assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1))
    assertEquals(Cons(30, Nil()), drop(lst, 2))
    assertEquals(Nil(), drop(lst, 5))

  @Test def appendTest(): Unit =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(lst, tail))
    assertEquals(lst, append(lst, Nil()))
    assertEquals(lst, append(Nil(), lst))
    assertEquals(Nil(), append(Nil(), Nil()))

  @Test def flatMapTest(): Unit =
    assertEquals(Nil(), flatMap(Nil())(v => Nil()))
    assertEquals(flatLst, flatMap(lst)(v => Cons(v + 1, Nil())))
    assertEquals(flat2Lst, flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def mapTest(): Unit =
    assertEquals(flatLst, map(lst)(_ + 1))
    assertEquals(Nil(), map(Nil())(v => Nil()))

  @Test def filterTest(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(lst)(_ >= 20))
    assertEquals(Nil(), filter(Nil())(v => v == Nil()))

  @Test def testMax(): Unit =
    assertEquals(Some(25), max(maxLst))
    assertEquals(None(), max(Nil()))

  /**
   * Tests of Task part 2
   */

  @Test def getCoursesTest(): Unit =
    val tLst = Cons(Teacher(t1Name, c1), Cons(Teacher(t1Name, c3), Cons(Teacher(t1Name, c2),
      Cons(Teacher(t2Name, c2), Cons(Teacher(t2Name, c3), Nil())))))
    assertEquals(Cons(c1, Cons(c2, Cons(c3, Nil()))), getCourses(tLst))
    assertEquals(Nil(), getCourses(Nil()))

  @Test def getCoursesFlatmapTest(): Unit =
    val tLst = Cons(Teacher(t1Name, c1), Cons(Teacher(t1Name, c3), Cons(Teacher(t1Name, c2),
      Cons(Teacher(t2Name, c2), Cons(Teacher(t2Name, c3), Nil())))))
    assertEquals(Cons(c1, Cons(c2, Cons(c3, Nil()))), getCoursesFlatmap(tLst))
    assertEquals(Nil(), getCoursesFlatmap(Nil()))

  @Test def foldLeftTest(): Unit =
    assertEquals(-16, foldLeft(foldlst)(foldReduce)(foldOp))
    assertEquals(foldReduce, foldLeft(Nil())(foldReduce)(foldOp))

  @Test def foldRightTest(): Unit =
    assertEquals(-8, foldRight(foldlst)(foldReduce)(foldOp))
    assertEquals(foldReduce, foldRight(Nil())(foldReduce)(foldOp))
}
