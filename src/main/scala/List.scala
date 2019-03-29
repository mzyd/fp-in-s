sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List extends App {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val l = List(1, 2, 3, 4, 5)

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_,t) => t
    }

  def setHead[A](l: List[A], head: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) => Cons(head, t)
    }

  // 删除 '前缀' 符合条件的元素
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  // val l2 = dropWhile(l, (a: Int) => a <= 3)
  // println("l2 ===> ", l2)

  def append[A](a1: List[A], a2: List[A]): List[A] = {

    println("a1 -> ", a1, " a2 -> ", a2)
    a1 match {
      case Nil => a2
      case Cons(h, t) => {
        Cons(h, append(t, a2))
      }
    }
  }

  // val l2 = List(3, 3, 3)
  // val l3 = append(l, l2)
  // println("=========", l3)

  // a1 = List(1, 2), a2 = List(3, 3)
  // Cons(1, append(2, a2))
  // Cons(1, Cons(2, append(t, a2)))
  // Cons(1, Cons(2, a2))

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }


  // Cons(1, 2, 3) => Cons( 1, init(Cons(2, 3)) )
  // Cons(2, 3) =>    Cons( 1, Cons(2, init(3)) )
  //                  Cons( 1, Cons(2, Nil) )

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if (f(h)) => dropWhile2(t)(f)
      case _ => l
    }

  // val l2 = dropWhile2(l)((a) => a < 3)
  // println("l2 =====> ", l2)

  def sum2(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum2(t)
  }

  def product2(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product2(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum3(ns: List[Int]) =
    foldRight(ns, 0)( (a, b) => a + b )

  // 1 + foldRight(Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))((a, b) => a + b)
  // 1 + (2, )

  def product3(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

}

