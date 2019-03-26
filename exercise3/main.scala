package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  // パターンマッチ入門
  def patternMatcher(l: List[Int]): Int = {
    l match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
  }

  // tailを実装
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  // 先頭を置き換える関数を実装
  def setHead[A](h: A, l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(h, xs)
    }
  }

  // 先頭からn個の要素を削除する関数を実装
  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (Cons(x, xs), 0) => Cons(x, xs)
      case (Nil, _) => Nil
      case (Cons(_, t), _) => drop(t, n - 1)
    }
  }

  // 要素とマッチするまで削除し続ける関数を実装
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  // 末尾要素を削除する関数を実装
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // lengthを実装
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  // foldLeftを実装
  // def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
  //   as match {
  //     case Nil => z
  //     case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  //   }
  // }
  // ex)
  // foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y)
  // 1 + foldRight(Cons(2, Cons(3, Nil)), 0)((x, y) => x + y)
  // 1 + 2 + foldRight(Cons(3, Nil), 0)((x, y) => x + y)
  // 1 + 2 + 3 + foldRight(Nil, 0)((x, y) => x + y)
  // 1 + 2 + 3 + 0
  // 6
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }
  // ex)
  // foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y)
  // foldLeft(Cons(2, Cons(3, Nil)), (0 + 1))((x, y) => x + y)
  // foldLeft(Cons(3, Nil), (0 + 1 + 2))((x, y) => x + y)
  // foldLeft(Nil, (0 + 1 + 2 + 3))((x, y) => x + y)
  // 0 + 1 + 2 + 3
  // 6

  // foldLeftを使ってsum,productを実装
  def sumLeft(as: List[Int]): Int = {
    foldLeft(as, 0)((x, y) => x + y)
  }
  def productLeft(as: List[Int]): Int = {
    foldLeft(as, 1)((x, y) => x * y)
  }

  // 要素を逆順にする実装
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((x, y) => Cons(y, x))
  }

  // appendを実装
  def append[A](as: List[A], r: List[A]): List[A] = {
    foldRight(as, r)(Cons(_, _))
  }

  // concatを実装
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append(_, _))
  }

  // 各要素に1を足したリストを返す関数を実装
  def eachInc(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))
  }

  // 各要素をstring型に変換したリストを返す関数を実装
  def eachToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  // mapを実装
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  // filterを実装
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  // flatMapを実装
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  // filter(by flatMap)を実装
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  // 要素同士を加算したリストを返却する関数を実装
  def matrixAdd(a: List[Int], b: List[Int]): List[Int] = {
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, matrixAdd(t1, t2))
    }
  }

  // 要素同士に関数を適用したリストを返却する関数を実装
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

}


object Exercise {
  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4, 5)

    // 3.1 パターンマッチ入門
    println("--- 3.1 ----------")
    println(List.patternMatcher(l))

    // 3.2 tail関数実装
    println("--- 3.2 ----------")
    val t = List.tail(l)
    println("%s tail is %s.".format(l, t))

    // 3.3 setHead関数実装
    println("--- 3.3 ----------")
    val h = List.setHead(10, l)
    println("%s setHead(10) is %s.".format(l, h))

    // 3.4 drop関数実装
    println("--- 3.4 ----------")
    val d = List.drop(l, 2)
    println("%s drop(2) is %s.".format(l, d))

    // 3.5 dropWhile関数実装
    println("--- 3.5 ----------")
    val dw = List.dropWhile(l, (a: Int) => a < 4)
    println("%s dropWhile(4) is %s.".format(l, dw))

    // 3.6 init関数実装
    println("--- 3.6 ----------")
    val i = List.init(l)
    println("%s init is %s.".format(l, i))

    // 3.9 length関数実装
    println("--- 3.9 ----------")
    val sz = List.length(l)
    println("%s length is %s.".format(l, sz))

    // 3.11 foldLeft関数実装
    println("--- 3.11 ----------")
    val sl = List.sumLeft(l)
    println("%s sum is %s.".format(l, sl))
    val pl = List.productLeft(l)
    println("%s product is %s.".format(l, pl))

    // 3.12 reverse関数実装
    println("--- 3.12 ----------")
    val r = List.reverse(l)
    println("%s reverse is %s.".format(l, r))

    // 3.14 append関数実装
    println("--- 3.14 ----------")
    val ap = List.append(l, List(11,13))
    println("%s append is %s.".format(l, ap))

    // 3.15 concat関数実装
    println("--- 3.15 ----------")
    val ml = List(List(1, 3), List(2, 4))
    val c = List.concat(ml)
    println("%s concat is %s.".format(ml, c))

    // 3.16 eachInc関数実装
    println("--- 3.16 ----------")
    val ei = List.eachInc(l)
    println("%s eachInc is %s.".format(l, ei))

    // 3.17 eachToString関数実装
    println("--- 3.17 ----------")
    val dl = List(1.1, 2.2, 3.3, 4.4, 5.5)
    val es = List.eachToString(dl)
    println("%s eachToString is %s.".format(dl, es))

    // 3.18 map関数実装
    println("--- 3.18 ----------")
    val m = List.map(l)(_ * 10)
    println("%s map is %s.".format(l, m))

    // 3.19 filter関数実装
    println("--- 3.19 ----------")
    val f = List.filter(l)(_ % 2 == 0)
    println("%s filter is %s.".format(l, f))

    // 3.20 flatMap関数実装
    println("--- 3.20 ----------")
    val fm = List.flatMap(l)(a => List(a, a))
    println("%s flatMap is %s.".format(l, fm))

    // 3.21 filter2関数実装
    println("--- 3.21 ----------")
    val f2 = List.filter2(l)(_ % 2 == 0)
    println("%s filter is %s.".format(l, f2))

    // 3.22 matrixAdd関数実装
    println("--- 3.22 ----------")
    val ma = List.matrixAdd(l, l)
    println("%s filter matrixAdd %s.".format(l, ma))

    // 3.23 zipWith関数実装
    println("--- 3.23 ----------")
    val z = List.zipWith(l, l)((a, b) => a + b)
    println("%s filter matrixAdd %s.".format(l, z))

  }
}
