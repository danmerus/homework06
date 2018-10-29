package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

object Eq {
    implicit def intEq[Int]: Eq[Int] = new Eq[Int] {
      def equiv(lft: Int, rgt: Int):Boolean = lft == rgt
    }
    implicit def mapEq[A: Eq]: Eq[Map[A,A]] = new Eq[Map[A,A]]{
      def equiv(lft: Map[A,A], rgt: Map[A,A]): Boolean = {
        val eqA = implicitly[Eq[A]]
        lft == rgt
      }
    }
    implicit def seqEq[A: Eq]: Eq[Seq[A]] = new Eq[Seq[A]]{
      def equiv(lft: Seq[A], rgt: Seq[A]): Boolean = {
        val eqA = implicitly[Eq[A]]
        lft == rgt
        }
      }
    implicit def optionEq[A: Eq]: Eq[Option[A]] = new Eq[Option[A]]{
      def equiv(lft: Option[A], rgt: Option[A]): Boolean = {
        val eqA = implicitly[Eq[A]]
        lft == rgt
      }
    }
}