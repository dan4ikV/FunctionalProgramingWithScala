import com.sun.source.tree.BinaryTree

//task1
val fib: () => LazyList[Int] = () => 0 #:: 1 #:: fib().zip(fib().tail).map(n => n._1 + n._2)
fib().take(10).toList
fib().take(20).toList
fib()(20)

//task2
sealed trait BinaryTree[+A]

case object Empty extends BinaryTree[Nothing]

case class Node[A](value: A, left: () => BinaryTree[A], right: () => BinaryTree[A]) extends BinaryTree[A]

def getValue[A](tree: BinaryTree[A]): Option[A] = tree match {
  case Empty => None
  case Node(a, _, _) => Some(a)
}

val ITree: (Int) => BinaryTree[Int] = (n: Int) => Node(n, () => ITree(2 * n), () => ITree(2 * n + 1))

def IBredth[A](binaryTree: BinaryTree[A]): LazyList[A] = {
  def goBredth(queue: List[BinaryTree[A]], newQueue: List[BinaryTree[A]]): LazyList[A] = queue match {
    case Nil => if (newQueue.isEmpty) LazyList[A]() else goBredth(newQueue, List[Node[A]]())
    case h :: t => h match {
      case Node(value, left, right) => value #:: goBredth(t, newQueue ::: List(left()) ::: List(right()))
      case Empty => goBredth(t, newQueue)
    }
  }

  goBredth(List(binaryTree), List[BinaryTree[A]]())
}

IBredth(ITree(1)).take(30).toList
IBredth(ITree(5)).take(30).toList
IBredth(ITree(5)).take(0).toList
IBredth(ITree(-1)).take(1).toList
IBredth(ITree(-1)).take(20).toList

//task3
sealed trait LList[+T]
case object LNil extends LList[Nothing]
case class LCons[U](head: U, tail: () => LList[U]) extends LList[U]

def takeL[T](llist: LList[T], n: Int): List[T] =
  (llist, n) match {
    case (LNil, _) => Nil
    case (_, k) if k <= 0 => Nil
    case (LCons(h, t), _) => h :: takeL(t(), n - 1)
  }

def mapL[T, U](llist: LList[T])(f: T => U): LList[U] =
  llist match {
    case LNil => LNil
    case LCons(h, t) => LCons(f(h), () => mapL(t())(f))
  }
/*
sealed trait LListMemo[+T]
case object LNilMemo extends LListMemo[Nothing]
case class LConsMemo[U](head: U, tail: () => LListMemo[U]) extends LListMemo[U]{
  def apply(): Unit ={
    tail()
  }
}

def takeLMemo[T](llist: LListMemo[T], n: Int): List[T] =
  (llist, n) match {
    case (LNilMemo, _) => Nil
    case (_, k) if k <= 0 => Nil
    case (LConsMemo(h, t), _) => h :: takeLMemo(t(), n - 1)
  }

def toLLMemo[A](list: LList[A]): LListMemo[A] = list match {
  case LNil => LNilMemo
  case LCons(h, t) => val elem = LConsMemo(h, t); t.apply(); elem
}

val ones: LListMemo[Int] = LConsMemo(1, () => ones)
 */
def memoize[I, O](f: I => O): I => O = new scala.collection.mutable.HashMap[I, O]() {
  override def apply(key: I) = getOrElseUpdate(key, f(key))
}

sealed trait LListMemo[+T]
case object LNilMemo extends LListMemo[Nothing]
case class LConsMemo[U](head: U, tail: () => LListMemo[U]) extends LListMemo[U]{
  lazy val apply:() => LListMemo[U] /*(() => LListMemo[U], LListMemo[U])*/ = memoize {
    tail()
  }
}

var memo = scala.collection.mutable.Map[() => LListMemo[Any], LListMemo[Any]]()

/*var memo = memoization[LListMemo[Int]]()
val ones: LListMemo[Int] = {
  memo += LConsMemo(1, () => ones).apply()._1 -> LConsMemo(1, () => ones).apply()._2
  LConsMemo(1, () => ones)
}

ones
 */

val ones: LListMemo[Int] ={val temp = LConsMemo(1, () => ones); memo += temp.apply()._1 -> temp.apply()._2; temp}
