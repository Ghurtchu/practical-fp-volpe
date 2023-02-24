object Typeclass {

  trait Monoid[A] {
    def combine(left: A, right: A): A
    def empty: A
  }

  object Monoid {
    def apply[A](implicit semigroup: Monoid[A]): Monoid[A] = semigroup
  }

  object MonoidInstances {
    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      override def combine(left: Int, right: Int): Int = left + right
      override def empty: Int = 0
    }
  }

  implicit class MonoidSyntax[A: Monoid](self: A) {
    def <+>(that: A): A = Monoid[A].combine(self, that)
  }

  def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.fold(monoid.empty)(monoid.combine)

  def combineAll2[A: Monoid](list: List[A]): A = list.fold(Monoid[A].empty)(Monoid[A].combine)

  def combineAll3[A: Monoid](list: List[A]): A = list.fold(Monoid[A].empty)(_ <+> _)
}
