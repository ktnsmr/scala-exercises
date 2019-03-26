object Exercise {
  // フィボナッチ数列
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, pre: Int, cur: Int) : Int =
      if (n == 1) 0
      else if (n == 2) cur
      else go(n - 1, cur, pre + cur)

    go(n, 0, 1)
  }

  // ソート済みか判定
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int) : Boolean =
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else go(n + 1)

    go(0)
  }

  private def exec2_2(as: Array[Int]): Unit = {
    val ret = isSorted(as, (cur: Int, next: Int) => next >= cur)
    val msg = "%s is%s sorted."
    println(msg.format(as.deep, (if(ret) "" else " not")))
  }

  // カリー化
  def curry[A, B ,C](f: (A, B) => C): A => (B => C) = {
    a => { b => f(a, b) }
  }

  // 逆カリー化
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => { f(a)(b) }
  }

  // 関数の合成
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
  }



  def main(args: Array[String]): Unit = {
    // 2.1 フィボナッチ数列
    println("--- 2.1 ----------")
    1.to(10).map{x => println(fib(x)) }

    // 2.2 ソート済みか判定
    println("--- 2.2 ----------")
    exec2_2(Array(1, 2, 2, 3))
    exec2_2(Array(1, 2, 1, 3))
    exec2_2(Array(1))

    // 2.3 カリー化
    println("--- 2.3 ----------")
    val add = curry((a: Int, b: Int) => a + b)
    println(add)
    val ++ = add(1)
    println(++(100))

    // 2.4 逆カリー化
    println("--- 2.4 ----------")
    val -- = uncurry((a: Int) => (b: Int) => a - b)
    println(--)
    println(--(100, 1))

    // 2.5 関数の合成
    println("--- 2.5 ----------")
    val circleArea = compose((b: Int) => b * 3.14, (a: Int) => a * a)
    println(circleArea)
    println(circleArea(10))
  }
}
