import scala.annotation.tailrec

//task 1
def reverse[A, B](tup: (A, B)): (B, A) = {
  (tup._2, tup._1)
}

reverse(("A", "B"))
reverse((1, "B"))
reverse(("3", reverse(2, 1)))

//task 2
def secondElement[A](list: List[A]): Option[A] = {
  if (list.length >= 2) {
    Some(list(1))
  }
  else {
    None
  }
}

secondElement(List.range(-1, 1))
secondElement(List.range(1, 10))
secondElement(List.range(1, 0))
secondElement(List())
secondElement(List(None, None))

//task 3
def length[A](list: List[A]): Int = list match {
  case Nil => 0
  case _ :: tail => length(tail) + 1
}

length(List.range(0, 10))
length(List())
length(List((1, 2)))

//task 4
def reversedList[T](list: List[T]): List[T] = list match {
  case Nil => list
  case x :: tail =>
    reversedList(tail) ::: List(x)
}


def reversedListTailRecursion[T](list: List[T]): List[T] = {
  @tailrec
  def go(list: List[T], accumulatorList: List[T]): List[T] = list match {
    case Nil => accumulatorList
    case x :: tail => go(tail, List(x) ::: accumulatorList)
  }

  go(list, List())
}

reversedList(List())
reversedList(List(reversedListTailRecursion(List()), reversedListTailRecursion(List(5, 4, 3, 2, 1))))

//task 6
def fibRec(num: Int): Int = num match {
  case a if a < 0 => print("number should be positive")
    -1
  case 0 => 0
  case 1 => 1
  case _ => fibRec(num - 1) + fibRec(num - 2)
}

def fibTailrec(num: Int): Int = {
  @tailrec
  def go(prev: Int, accum: Int, counter: Int, num: Int): Int = counter match {
    case a if a < 0 =>
      print("Number should be non negative!")
      -1
    case a if a <= num =>
      counter match {
        case 0 => go(0, 0, counter + 1, num)
        case 1 => go(0, 1, counter + 1, num)
        case _ => go(accum, accum + prev, counter + 1, num)
      }
    case _ => accum
  }

  go(0, 0, 0, num)
}

fibRec(2)
fibTailrec(2)
fibRec(4)
fibTailrec(4)
fibRec(-4)
fibTailrec(50)
//fibRec(50)

//task 7
def powersSet[A](list: List[A]): List[List[A]] = list match {
  case Nil => List(List())
  case _ =>
    def append(list2: List[List[A]], elem: A): List[List[A]] = list2 match {
      case Nil => List()
      //case a if a.size == 1 => List(elem :: list2.head)
      case h :: t => List(elem :: h) ::: append(t, elem)
    }
    val a = powersSet(list.tail)
    append(a, list.head) ::: a
}

powersSet(List(1, 2, 3, 4))
powersSet(List())
powersSet(List(1))

//pattern matching task 6
def secondElementPat[A](list: List[A]): Option[A] = list match{
  case a if a.length >= 2 =>
    Some(list(1))
  case _ =>
    None
}

secondElementPat(List.range(-1, 1))
secondElementPat(List.range(1, 10))
secondElementPat(List.range(1, 0))
secondElementPat(List())
secondElementPat(List(None, None))

//pattern matching task 7
def replaceN[T](list: List[T], n : Int, element: T): List[T] = list match{
  case Nil => list
  case x :: tail => if(n == 0) element :: tail
  else x :: replaceN(tail, n - 1, element)
}

replaceN(List.range(1, 10), 5, 0)
replaceN(List.range(1, 10), 10, 0)
replaceN(List(), 10, 0)

//higher order functions task 8
def map[A, B](func : A => B, list : List[A]): List[B] = list match {
  case Nil => List()
  case h :: t => func(h) :: map(func, t)
}

val addWord = (a : String) => "word " + a
map(addWord, List("1", "2", "3"))
val squareOdds = (a : Int) => if((a % 2) == 1) {a * a} else a
map(squareOdds, List.range(0, 10))

//higher order functions task 9

def split[A](list : List[A]): (List[A], List[A]) = {
  def twoLists(list1 : List[A], list2 : List[A]): (List[A], List[A]) = (list1 , list2) match {
    case (a , b) if a.size <= b.size => (a , b)
    case _ => twoLists(list1.tail , list1.head :: list2)
  }
  twoLists(list, List())
}

def merge[A](list1 : List[A], list2: List[A], comparator: (A, A) => A): List[A] = {
  @tailrec
  def mergeTail(list1 : List[A], list2: List[A], accumulator : List[A], comparator: (A , A) => A): List[A] = (list1, list2) match {
    case(Nil, a) => accumulator ::: a
    case(a, Nil) => accumulator ::: a
    case _ => comparator(list1.head, list2.head) match {
      case a if a == list1.head => (mergeTail(list1.tail, list2, accumulator ::: List(a), comparator))
      case a if a == list2.head => (mergeTail(list1, list2.tail, accumulator ::: List(a), comparator))
    }
  }
  mergeTail(list1, list2, List(), comparator)
}

def mergeSort[A](list : List[A], comparator: (A, A) => A): List[A] = list match{
  case a if a.size == 0 => List()
  case a if a.size == 1 => a
  case _ =>
    val twoLists = split(list)
    merge(mergeSort(twoLists._1, comparator), mergeSort(twoLists._2, comparator), comparator)
}

val comparator = (a: Int, b : Int) => if(a < b) a else b
mergeSort(List(1, 3, 2, 4, 3, 5, 4, 6), comparator)
mergeSort(List(2, 1), comparator)

def countOccurences[A](list: List[A], elem: A): Int = list match {
  case Nil => 0
  case h :: t => if(h == elem) countOccurences(t, elem) + 1 else countOccurences(t, elem)
}

//testing
countOccurences(List(1, 2, 2, 4, 1, 2), 2)
countOccurences(List.empty, 2)
countOccurences(List("a", "b", "c", "d"), "a")
countOccurences(List("a", "b", "c", "d"), 2)

@tailrec
def checkSorted(list: List[Int]): Boolean = list match{
  case h :: t if (t.size == 1) => if(h < t(0)) true else false
  case h :: t => if(h < t.head) checkSorted(t) else false
  case Nil => true
}

//testing
checkSorted(List(1, 2 ,3, 4, 5))
checkSorted(List(1, 2 ,3, 5, 4))
checkSorted(List())

def occurences(num: Int, list: List[Int]): Int = list match {
  case Nil => 0
  case head :: tail => if (head == num) occurences(num, tail) + 1 else occurences(num, tail)
}

def isSorted(list: List[Int]): Boolean = list match {
  case head :: tail1 :: tail2 => if(head < tail1) isSorted(tail1 :: tail2) else false
  case _ => true
}

isSorted(List(1, 2 ,3, 4, 5))
isSorted(List(1, 2 ,3, 5, 4))
isSorted(List())
