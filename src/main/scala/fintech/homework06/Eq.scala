package fintech.homework06
import scala.math._

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

case class Complex(re: Double, im: Double, epsilon: Double) extends Ordered[Complex] {
  private val module = sqrt(pow(re, 2) + pow(im, 2))

  def this(re: Double) = this(re, 0, 0)

  def +(c: Complex) = Complex(re + c.re, im + c.im, epsilon)
  def -(c: Complex): Complex = this + c*(-1)
  def *(c: Int) =
    Complex(re * c, im * c, epsilon)
  def *(c: Complex) =
    Complex(re * c.re - im * c.im, im * c.re + re * c.im, epsilon)

  override def compare(that: Complex): Int = this.module compare that.module
}
trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

object Eq {
  implicit def intEq[Int]: Eq[Int] = new Eq[Int] {
      def equiv(lft: Int, rgt: Int):Boolean = lft == rgt
    }
  implicit def strEq[String]: Eq[String] = new Eq[String] {
    def equiv(lft: String, rgt: String):Boolean = lft == rgt
  }
  implicit def doubleEq[Double]: Eq[Double] = new Eq[Double] {
    def equiv(lft: Double, rgt: Double):Boolean = {
      lft == rgt
    }

  }
  implicit def complexEq[Complex]: Eq[Complex] = new Eq[Complex] {
    def equiv(lft: Complex, rgt: Complex):Boolean = lft == rgt
  }
    implicit def mapEq[A: Eq]: Eq[Map[A,A]] = new Eq[Map[A,A]]{
      def equiv(lft: Map[A,A], rgt: Map[A,A]): Boolean = {
        val eqA = implicitly[Eq[A]]
        var  out = true
        if (lft.size != rgt.size) {
          return false
        }
        for ((k,v) <- lft){
          out = out && eqA.equiv(lft(k), rgt(k))
        }
        out
      }
    }
    implicit def seqEq[A: Eq]: Eq[Seq[A]] = new Eq[Seq[A]]{
      def equiv(lft: Seq[A], rgt: Seq[A]): Boolean = {
        val eqA = implicitly[Eq[A]]
        var out = true
        if (lft.size != rgt.size) {
          return false
        }
        for (i <- lft.indices) {
          out = out && eqA.equiv(lft(i), rgt(i))
        }
        out
        }
      }
    implicit def optionEq[A: Eq]: Eq[Option[A]] = new Eq[Option[A]]{
      def equiv(lft: Option[A], rgt: Option[A]): Boolean = {
        val eqA = implicitly[Eq[A]]
        lft == rgt
      }
    }
  implicit class EqOps[T: Eq](val self: T) {
    def ===(other: T): Boolean = implicitly[Eq[T]].equiv(self, other)
  }
}
