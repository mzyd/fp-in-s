sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons1[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream extends App {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons1(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}
