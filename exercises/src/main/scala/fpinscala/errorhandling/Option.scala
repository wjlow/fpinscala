package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap2[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def orElse2[B>:A](ob: => Option[B]): Option[B] = map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap { m =>
      val doubles: Seq[Double] = xs map { x =>
        math.pow(x - m, 2)
      }
      mean(doubles)
    }
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }

  def map22[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((optA, optBs) => (optA, optBs) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(aa), Some(bs: List[A])) => Some(bs :+ aa)
    })

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))(map2(_, _)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def loop[A, B](acc: Option[List[B]])(a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case Nil => acc
      case (h::t) if f(h) != None =>
        val newAcc: Option[List[B]] = acc flatMap (acc2 => f(h) map (acc2 :+ _))
        loop(newAcc)(t)(f)
      case _ => None
    }
    loop(Some(Nil))(a)(f)
  }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse2(t)(f))(_ :: _)
  }

  def traverse3[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => for {
      fh <- f(h)
      tr <- traverse3(t)(f)
    } yield fh :: tr
  }

  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse3(a)(aa => aa)
}