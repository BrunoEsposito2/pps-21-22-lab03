package tasks

import u03.Streams.{Stream}
import u03.Lists.List.*
import org.junit.Test
import org.junit.Assert.*

class StreamTaskTest {

  import u03.Lists.List.{Cons, Nil}
  import u03.Streams.Stream.*

  val init = 0
  val tot = 10
  val f: (Int) => Int = _ + 1
  val s = Stream.take(Stream.iterate(init)(f))(tot)

  @Test def dropTest(): Unit =
    val start = 6
    val expected = Stream.iterate(start)(f)
    val actual = Stream.drop(s)(start)
    assertEquals(Stream.toList(Stream.take(expected)(tot-start)), Stream.toList(actual))

  @Test def constantTest(): Unit =
    val c = "x"
    assertEquals(Cons(c, Cons(c, Cons(c, Cons(c, Cons(c, Nil()))))),
      Stream.toList(Stream.take(constant(c))(5)))

  @Test def fibsTest(): Unit =
    val firstValues = 8
    assertEquals(Cons(0,Cons(1,Cons(1,Cons(2,Cons(3,Cons(5,Cons(8,Cons(13,Nil())))))))),
      Stream.toList(Stream.take(fibs)(firstValues)))

}
