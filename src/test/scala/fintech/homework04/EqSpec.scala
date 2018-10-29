package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}

class EqSpec extends FlatSpec with Matchers {
  it should "compare maps correctly 1" in {
    val m1 = Map("a" -> 2)
    val m2 = Map("a" -> 2)
    m1 === m2 shouldEqual true
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
}
