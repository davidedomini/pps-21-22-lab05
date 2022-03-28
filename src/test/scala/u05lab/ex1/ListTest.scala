package u05lab.ex1

import org.junit.Test
import org.junit.Assert.*
import u05lab.ex1.List

class ListTest {

  @Test
  def testZipRightWithRecursion(): Unit = {
    val l: List[Int] = List(10,20,30)
    assertEquals(List((10,0), (20,1), (30,2)), l.zipRightWithRecursion)
  }

  @Test
  def testZipRight(): Unit = {
    val l: List[Int] = List(10,20,30)
    assertEquals(List((10,0), (20,1), (30,2)), l.zipRight)
  }

  @Test
  def testPartition(): Unit = {
    val l = List(1, 2, 3, 4)
    assertEquals( (List(2,4), List(1, 3)), l.partition(_ % 2 == 0))
  }

  @Test
  def testSpan(): Unit = {
    val l = List(2, 1, 2, 3, 4)
    assertEquals( (List(2, 1), List(2, 3, 4)), l.partition(_ % 2 != 0))
  }


}
