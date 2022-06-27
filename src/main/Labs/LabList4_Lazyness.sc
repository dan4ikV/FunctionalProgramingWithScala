//TASK 1
def unzipL[A, B](listOfPairs: LazyList[(A, B)]): (LazyList[A], LazyList[B]) = listOfPairs match{
  case a if a.isEmpty => (LazyList[A](), LazyList[B]())
  case h #:: t => lazy val temp = unzipL(t); (h._1 #:: temp._1 , h._2 #:: temp._2)
}


//test1
val testList1 = unzipL(LazyList((1, "a"), (2, "b"), (3, "c")))._1
val testList2 = unzipL(LazyList((1, "a"), (2, "b"), (3, "c")))._2
testList1.take(3).toList
testList2.take(3).toList

//test2
unzipL(LazyList((testList1, testList2)))._1.take(3).toList

//test3
val ones: LazyList[Int] = 1 #:: ones
val nats:(Int) => LazyList[Int] = (n : Int) => n #:: nats(n + 1)
val zipped: LazyList[(Int, Int)] = ones.zip(nats(1))
unzipL(zipped)._1.take(10).toList
unzipL(zipped)._2.take(20).toList



//TASK 2
sealed trait LList[+T]
case object LNil extends LList[Nothing]
case class LCons[T](head: T, tail: () => LList[T]) extends LList[T]

/*def filterL[A](list: LList[A], predicate: (A) => Boolean): () => LList[A] = list match{
  case LNil => () => LNil
  case LCons(head, tail) => if(predicate(head) == true) () => LCons(head, filterL(tail(), predicate)) else filterL(tail(), predicate)
}*/

def filterL[A](list: LList[A], predicate: (A) => Boolean): LList[A] = list match{
  case LNil => LNil
  case LCons(head, tail) => if(predicate(head)) LCons(head, () => filterL(tail(), predicate))
  else filterL(tail(), predicate)
}

def takeL[T](llist: LList[T], n: Int): List[T] =
  (llist, n) match {
    case (LNil, _) => Nil
    case (_, k) if k <= 0 => Nil
    case (LCons(h, t), _) => h :: takeL(t(), n - 1)
  }

//test1
val naturalNums: Int => LList[Int] = (n : Int) => LCons(n, () => naturalNums(n + 1))
val odds: Int => Boolean = (num : Int) => num % 2 == 1
takeL(filterL(naturalNums(1), odds), 20)
takeL(filterL(naturalNums(30), odds), 1)
takeL(filterL(naturalNums(30), odds), 30)

//test2
val posAndNeg: Int => LList[Int] = (n : Int) => if(n >= 0) LCons(n, () => posAndNeg(-(n + 1)))
else LCons(n, () => posAndNeg(-(n - 1)))
val onlyNegs: Int => Boolean = (num : Int) => num < 0
takeL(filterL(posAndNeg(1), onlyNegs), 20)
takeL(filterL(posAndNeg(30), onlyNegs), 1)
takeL(filterL(posAndNeg(30), onlyNegs), 30)