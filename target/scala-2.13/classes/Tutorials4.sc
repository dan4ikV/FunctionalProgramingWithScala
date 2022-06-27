//task 1
def lrepeat[A](k: Int)(lxs: LazyList[A]): LazyList[A] = lxs match{
  case LazyList() => LazyList.empty
  case h #:: t => LazyList.fill(k)(h) #::: lrepeat(k)(t)
}

val repeat = lrepeat(3)(LazyList.range(1, 100).take(20))
print(repeat(2))
print(repeat(5))
print(repeat(8))
print(repeat(11))
print(repeat(14))
print(repeat(17))

//task 2
def fib(): LazyList[Int] = 0 #:: 1 #:: fib().zip(fib().tail).map {n => n._1 + n._2 }

fib()(2)
fib()(3)
fib()(13)
fib()(30)

//task 3
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem:A, left:()=>lBT[A], right:()=>lBT[A]) extends lBT[A]

def getLeft[A](tree : lBT[A]): lBT[A] = tree match {
  case LEmpty => LEmpty
  case LNode(elem, left, right) => left()
}

def getRight[A](tree : lBT[A]): lBT[A] = tree match {
  case LEmpty => LEmpty
  case LNode(elem, left, right) => right()
}

val t : (Int)=>(()=>LNode[Int]) = (n : Int) => ()=>LNode(n, t(n * 2), t(n * 2 + 1))
val root = t(1)()
getLeft(root)
getRight(root)
getLeft(getLeft(root))
getRight(getLeft(root))
getLeft(getRight(root))
getRight(getRight(root))