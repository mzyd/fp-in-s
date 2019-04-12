// sealed trait Option[+A]
// case class Some[+A](get: A) extends Option[A]
// case object None extends Option[Nothing]

// trait Option[+A] {

//   def map[B](f: A => B): Option[B]

//   def flatMap[B](f: A => Option[B]): Option[B]

//   // default: => 表示参数类型是 B, 但不是立即求值, 而是函数需要时才求值
//   // B >: A 表示 B 类型的参数必须是 A 的父类型
//   def getOrElse[B >: A](default: => B): B

//   def orElse[B >: A](ob: => Option[B]): Option[B]

//   def filter(f: A => Boolean): Option[A]

// }

// object Chapter extends App {
// object Chapter {

  // def failingFn(i: Int): Int = {
  // val y: Int = throw new Exception("fail!")

  //   try {
  //     val x = 42 + 5
  //     x + ((throw new Exception("fail!")): Int)
  //   } catch { case e: Exception => 43 }
  // }

  // println("========", failingFn(2) )

  // def mean(xs: Seq[Double]): Option[Double] =
  //   if (xs.isEmpty) None
  //   else Some(xs.sum / xs.length)

// }
