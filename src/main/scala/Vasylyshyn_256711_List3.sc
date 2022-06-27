import scala.annotation.tailrec

def sumList(list: List[Int]): Int = {
  def go(list: List[Int], accumulator: Int): Int = list match {
    case Nil => accumulator
    case x :: tail => go(tail, accumulator + x)
  }
  go(list, 0)
}

def reversedList[T](list: List[T]): List[T] = list match {
  case Nil => list
  case x :: tail =>
    reversedList(tail) ::: List(x)
}

def reversedListTailRecursion[T](list: List[T]): List[T] = {
  def go(list: List[T], accumulatorList: List[T]): List[T] = list match {
    case Nil => accumulatorList
    case x :: tail => go(tail, List(x) ::: accumulatorList)
  }
  go(list, List())
}

def mergeLists[T](list1: List[T], list2: List[T]): List[T] = {
  def go(list1: List[T], list2: List[T], accumulatorList: List[T]): List[T] = {
    list1 match {
      case x :: tail =>
        list2 match {
          case y :: tail2 =>
            go(tail, tail2, accumulatorList ::: List(x) ::: List(y))
          case Nil => go(tail, list2, accumulatorList ::: List(x))
        }
      case Nil =>
        list2 match {
          case x :: tail => go(list1, tail, accumulatorList ::: List(x))
          case Nil => accumulatorList
        }
    }
  }
  go(list1, list2, List())
}

def fibTailrec(num: Int): Int ={
  def go(prev: Int, accum: Int, counter: Int, num: Int): Int = {
    if(counter <= num){
      counter match{
        case 0 => go(0, 0, counter + 1, num)
        case 1 => go(0, 1, counter + 1, num)
        case _ => go(accum, accum+prev, counter +1, num)
      }
    }
    else {
      accum;
    }
  }
  go(0, 0, 0, num)
}

def splitList(list: List[Int]): (List[Int] , List[Int]) = list match{
  case Nil => (List(), List())
  case x :: tail => if(x % 2 == 0) (x :: splitList(tail)._1, splitList(tail)._2)
    else{
    (splitList(tail)._1, x :: splitList(tail)._2)
  }
}

def checkOrder(list: List[Int]): Boolean ={
  def go(list: List[Int], prev: Int): Boolean = list match{
    case Nil => true
    case x :: tail => if(x >= prev) go(tail, x)
    else false
  }
  go(list, Integer.MIN_VALUE)
}

def replaceN[T](list: List[T], n : Int, element: T): List[T] = list match{
  case Nil => println("Index out of bound")
    list
  case x :: tail => if(n == 0) element :: tail
  else x :: replaceN(tail, n - 1, element)
}

def pressure(atm: Double)(system: String): Double = {
  if(system.toLowerCase == "psi") atm * 14.6956
  else if(system.toLowerCase() == "torr") atm * 760
  else if(system.toLowerCase() == "pa") atm * 101325
  else if(system.toLowerCase() == "bar") atm * 1.01325
  else{
    println("Invalid system")
    -1
  }
}

//task1
reversedList(List.range(1, 10))
//task1.2
reversedListTailRecursion(List.range(1, 10))

//task2
sumList(List.range(1, 8))

//task3
mergeLists(List.range(1, 3), List.range(8, 25))

//task4
fibTailrec(20)

//task5
splitList(List.range(1, 10))
splitList(List(1, 2, 5, 100, Math.pow(10, 3).toInt))

//task6
checkOrder(List(1, 2, 4, 3, 6, 3, 4))
checkOrder(List(1, 2, 5, 100, Math.pow(10, 3).toInt))
checkOrder(List())

//task7
replaceN(List.range(1, 10), 5, 0)
replaceN(List.range(1, 10), 10, 0)

//task8
var atm = pressure(12)_
print(atm("psi"))
print(atm("torr"))
print(atm("pa"))
print(atm("bar"))
print(atm("barpsi"))

