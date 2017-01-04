package dataMacro

import org.scalatest.FunSuite

class basicDataTests extends FunSuite {
  test("@data is just like case class") {
    @data
    class One(one: Int)

    val o = One(1)
    assert(o.one == 1)
    assert(o == One(1))
    assert(o.toString == "One(1)")
    assert(o.hashCode() == One(1).hashCode())
    assert((o match {
      case One(x) => x
    }) == 1)

    @data
    class A(b: Int, c: String)

    val a = A(10, "hello!")
    assert(a.b == 10)
    assert(a.c == "hello!")
    assert(a == A(10, "hello!"))
    assert(a != A(20, "bye!"))
    assert(a.toString == "A(10,hello!)")
    assert(a.hashCode() == A(10, "hello!").hashCode())
    assert((a match {
      case A(b, c) => (b, c)
    }) == (10, "hello!"))
  }

  test("@data ctor validation") {
    assertDoesNotCompile("@data class A(var b: Int, var c: String)")
    assertDoesNotCompile("@data class A(b: Int, c: String)(d: Double)")
    assertDoesNotCompile("@data class A(b: => Int, var c: String)")
    assertCompiles("@data class A(b: Int => Int, c: String)")
    assertCompiles("@data class A(b: java.util.Set[String] => java.util.Set[String], c: String)")
  }

  test("@data interning") {
    @data
    class Foo(b: Int, c: String)

    val foo = Foo(10, "hello!")
    assert(!(foo eq Foo(10, "hello!")))
    assert(!(foo eq Foo(20, "bye!")))
    assert(foo == Foo(10, "hello!"))
    assert(foo != Foo(20, "bye!"))

    @data(intern = true)
    class Bar(b: Int, c: String)

    val bar = Bar(10, "hello!")
    assert(bar eq Bar(10, "hello!"))
    assert(!(bar eq Bar(20, "bye!")))
    assert(bar == Bar(10, "hello!"))
    assert(bar != Bar(20, "bye!"))
    assert(!(bar eq new Bar(10, "hello!")))
    assert(bar eq new Bar(10, "hello!").intern)
  }
}
