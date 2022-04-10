package u05lab.ex1

import org.junit.Test
import org.junit.Assert.*
import u05lab.ex1.List

import scala.runtime.Nothing$

class ListTest {

  @Test
  def testZipRightWithRecursion(): Unit = {
    val l: List[Int] = List(10,20,30)
    assertEquals(List((10,0), (20,1), (30,2)), l.zipRightWithRecursion)
  }

  @Test
  def testZipRight(): Unit = {
    val l: List[Int] = List(10,20,30,40,50)
    val res = l.zipRight
    println(res)
    assertEquals(List((10,0), (20,1), (30,2), (40,3), (50,4)), res)
  }

  @Test
  def testPartition(): Unit = {
    val l = List(1, 2, 3, 4)
    assertEquals( (List(2,4), List(1, 3)), l.partition(_ % 2 == 0))
  }

  @Test
  def testSpan(): Unit = {
    val l = List(6, 1, 2, 3, 4)
    assertEquals( (List(6 , 1), List(2, 3, 4)), l.span(_ % 2 != 0))
  }

  @Test
  def testReduce(): Unit = {
    val l = List(1, 2, 3, 4)
    val l2 = List(1)
    val o = List()

    assertEquals(10, l.reduce(_+_).get)
    assertEquals(1, l2.reduce(_+_).get)
    assertTrue(o.reduce((e:Nothing,l:Nothing) => e).isEmpty)
  }

  @Test
  def testTakeRight(): Unit = {
    val l = List(1, 2, 3, 4)
    assertEquals(List(2, 3, 4), l.takeRight(3))
  }

}
