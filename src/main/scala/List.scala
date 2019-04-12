sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

// object List extends App {
object List {

  // def sum(ints: List[Int]): Int = ints match {
  //   case Nil => 0
  //   case Cons(x, xs) => x + sum(xs)
  // }

  // def product(ds: List[Double]): Double = ds match {
  //   case Nil => 1.0
  //   case Cons(0.0, _) => 0.0
  //   case Cons(x, xs) => x * product(xs)
  // }

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
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if (f(h)) => dropWhile2(t)(f)
      case _ => l
    }

  // val l2 = dropWhile2(l)((a) => a < 3)
  // println("l2 =====> ", l2)

  // def sum2(ints: List[Int]): Int = ints match {
  //   case Nil => 0
  //   case Cons(h, t) => h + sum2(t)
  // }

  // def product2(ds: List[Double]): Double = ds match {
  //   case Nil => 1.0
  //   case Cons(x, xs) => x * product2(xs)
  // }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // def sum3(ns: List[Int]) =
  //   foldRight(ns, 0)( (a, b) => a + b )

  // def product3(ns: List[Double]) =
  //   foldRight(ns, 1.0)(_ * _)

  // def length[A](as: List[A]): Int =
  //   foldRight(as, 0)((_, b) => b + 1)


  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def product(ns: List[Int]): Int = foldLeft(ns, 1)(_ * _)

  def sum(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((z, _) => z + 1)

  // val da = length(l)
  // println("========= ", da)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((z, h) => Cons(h, z))

  // def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
  //   foldLeft(reverse(l), z)((b,a) => f(a,b))

  // def append[A](a1: List[A], a2: List[A]): List[A] = {
  //   a1 match {
  //     case Nil => a2
  //     case Cons(h, t) => Cons(h, append(t, a2))
  //   }
  // }

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  // def appendViaFoldLeft[A](l: List[A], z: List[A]): List[A] =
  // foldLeft(l, z)(Cons())

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  // def addOne(l: List[Int]): List[Int] =
  //   l match {
  //     case Nil => Nil
  //     case Cons(h, t) => Cons(h + 1, addOne(t))
  //   }

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))


  // def doubleToString(ds: List[Double]): List[String] =
  //   ds match {
  //     case Nil => Nil
  //     case Cons(h, t) => Cons(h.toString, doubleToString(t))
  //   }

  def dToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((h, t) => Cons(h.toString, t))

  // def map[A, B](as: List[A])(f: A => B): List[B] =
  //   as match {
  //     case Nil => Nil
  //     case Cons(h, t) => Cons(f(h), map(t)(f))
  //   }

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((h, t) => Cons(f(h), t))

  // def filter[A](as: List[A])(f: A => Boolean): List[A] =
  //   as match {
  //     case Nil => Nil
  //     case Cons(h, t) if (f(h)) => filter(t)(f)
  //     case Cons(h, t) => Cons(h, filter(t)(f))
  //   }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // val c = filter(l)((a) => a % 2 == 0)
  // println("----------", c)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  //   as match {
  //     case Nil => Nil
  //     case Cons(h, t) => append(f(h), flatMap(t)(f))
  //   }

  // val a = flatMap(l)(a => List(a, a))
  // println("a  => ", a)

  def filterWithFlatmap[A, B](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
    @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }


}


// Cons(1.1, doubleToString(Cons(2.2, Nil)))
// Cons(1.1, Cons(2.2, doubleToString(Cons(Nil))))
// Cons(1.1, Cons(2.2, Nil))

// List(1, 2) - List(3, 3)
// Cons(1, foldRight(List(2, Nil), List(3, 3))(Cons(_, _)))
// Cons(1, Cons(2, foldRight(Nil), List(3, 3))(Cons(_, _)))
// Cons(1, Cons(2, Cons(3, 3)))

// sum3(l)
// 1 + foldRight(List(2, 3, 4, 5), 0)(_ + _)
// 1 + ( 2 + foldRight(List(3, 4, 5), 0)(_ + _) )
// 1 + ( 2 + (3 + foldRight(List(4, 5), 0)(_ + _) ) )
//

// List(1, 2, 0, 3)
// 1 * foldRight(List(0, 3), 1.0)(_ * _)
// 1 * ( 2 * foldRight(List(0, 3), 1.0)(_ * _) )
// 1 * ( 2 * ( 0 * foldRight(List(3), 1.0)(_ * _) ) )
// 1 * ( 2 * ( 0 * ( 3 * foldRight(Nil)(_ * _))))
// 1 * ( 2 * ( 0 * ( 3 * 1.0)))
