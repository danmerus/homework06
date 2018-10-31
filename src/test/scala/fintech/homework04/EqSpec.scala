package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.Complex

class EqSpec extends FlatSpec with Matchers {
  it should "compare maps correctly 1" in {
    val m1 = Map("a" -> 2)
    val m2 = Map("a" -> 2)
    m1 === m2 shouldEqual true
  }
  it should "compare str orrectly 1" in {
    val s1 = "abc"
    val s2 = "abc"
    s1 === s2 shouldEqual true
  }
  it should "compare maps correctly 2" in {
    val m1 = Map("a"->2)
    val m2 = Map("a"->2, "n"->2)
    m1 === m2 shouldEqual false
  }
  it should "compare maps correctly 3" in {
    val m1 = Map("a"->2)
    val m2 = Map("a"->3)
    m1 === m2 shouldEqual false
  }
  it should "compare seqs correctly 1" in {
    val l1 = List(1,2,3)
    val l2 = List(1,2,3)
    l1 === l2 shouldEqual true
  }
  it should "compare seqs correctly 2" in {
    val l1 = List(1,2,4)
    val l2 = List(1,2,3)
    l1 === l2 shouldEqual false
  }
  it should "compare option correctly 1" in {
    val o1 = Option(2)
    val o2 = Option(2)
    o1 === o2 shouldEqual true
  }
  it should "compare option correctly 2" in {
    val o1 = Option(1)
    val o2 = Option(2)
    o1 === o2 shouldEqual false
  }
  it should "compare Complex orrectly 1" in {
    val c1 = Complex(5.0, 0, 0)
    val c2 = Complex(5, 0, 0)
    c1 === c2 shouldEqual true
  }
}
