package dataMacro

import org.scalatest.FunSuite

import scala.util.Random

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

  test("@data equals by id") {
    @data(idEquals = true)
    class Bar(b: Int, c: String)

    val bar = Bar(10, "hello!")
    assert(!(bar eq Bar(10, "hello!")))
    assert(bar == Bar(10, "hello!"))
    assert(bar != Bar(20, "bye!"))
    //FIXME not complete tests
  }

  test("performance tests") {
    val numberOfClasses = 10
    val numberOfComparisons = 100000

    println(s"Total number of classes for comparisons: $numberOfClasses")
    println(s"Total number of comparisons: $numberOfComparisons")

    val data =
      Seq.fill(numberOfClasses)((Random.nextInt(), Random.nextString(1000000), Seq.fill(1000000)(Random.nextInt())))
    val comparingIdxs =
      Seq.fill(numberOfComparisons)((Random.nextInt(numberOfClasses), Random.nextInt(numberOfClasses)))
    val validComps = for ((i1, i2) <- comparingIdxs) yield data(i1).equals(data(i2))

    def timeInNs[R](block: => R): (R, Long) = {
      val start = System.nanoTime()
      val res = block
      (res, System.nanoTime() - start)
    }

    def measureComparisons[T](classes: Seq[T]): Double = {
      val tuples = for ((i1, i2) <- comparingIdxs)
        yield {
          val cl1 = classes(i1)
          val cl2 = classes(i2)
          val (r, eqTime) = timeInNs(cl1 == cl2)
          (r, eqTime)
        }
      assert(tuples.map(_._1) == validComps)
      tuples.map(_._2).sum.toDouble / numberOfComparisons
    }


    case class CaseClass(a: Int, b: String, c: Seq[Int])

    val caseClasses = data.map((CaseClass.apply _).tupled)
    val caseClassesTime = measureComparisons(caseClasses)
    println(s"Case class: mean time - $caseClassesTime ms")


    @data
    class DataClass(a: Int, b: String, c: Seq[Int])

    val dataClasses = data.map((DataClass.apply _).tupled)
    val dataClassesTime = measureComparisons(dataClasses)
    println(s"Data class: mean time - $dataClassesTime ms")

    @data(intern = true)
    class DataClassWithIntern(a: Int, b: String, c: Seq[Int])

    val dataClassesWithIntern = data.map((DataClassWithIntern.apply _).tupled)
    val dataClassesWithInternTime = measureComparisons(dataClassesWithIntern)
    println(s"Data class with interning: mean time - $dataClassesWithInternTime ms")

    @data(idEquals = true)
    class DataClassWithId(a: Int, b: String, c: Seq[Int])

    val dataClassesWithId = data.map((DataClassWithId.apply _).tupled)
    val dataClassesWithIdTime = measureComparisons(dataClassesWithId)
    println(s"Data class with equals by id: mean time - $dataClassesWithIdTime ms")
  }
}
