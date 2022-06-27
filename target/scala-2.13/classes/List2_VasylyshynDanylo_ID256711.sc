import scala.annotation.tailrec

def sumList(list: List[Int]): Int = list match {
  case Nil => 0
  case x :: tail => if (x % 2 == 0) sumList(tail)
  else x + sumList(tail)
}

def connectStrings(list: List[String], separator: String): String = list match {
  case Nil => println("empty list entered!")
    ""
  case x :: tail => if (tail != Nil) x + separator + connectStrings(tail, separator)
  else x
}

def occurrencesNumber(list: List[Any], element: Any): Int = list match {
  case Nil => 0
  case x :: tail => if (x == element) 1 + occurrencesNumber(tail, element)
  else occurrencesNumber(tail, element)
}

def fib(n: Int): Int = {
  if (n < 0) {
    println("To find the fibonacci number the argument should be >= 0")
    return -1
  }
  n match {
    case 0 | 1 => n
    case _ => fib(n - 1) + fib(n - 2)
  }
}

//task1
//testing with range [1, 8)
println(sumList(List.range(1, 8)))
//only even
println(sumList(List(2, 4, 6, 8)))
//only odd
println(sumList(List(3, 5, 7, 9)))

//task2
//just a simple test
val list3 = List("Danylo", "Illia", "Andriy", "Egor")
println(connectStrings(list3, ","))
//test the function to return warning when empty list is passed
val emptyList = List()
println(connectStrings(emptyList, ","))

//task3
//creating elements for the list
val name1 = "danylo"
val name2 = "illia"
val name3 = "andriy"
val list3_copy = List("Danylo", "Illia", "Andriy", "Egor")
val anyList = List(list3, name1, name2, name3, name1, 5)
//different tests
println(occurrencesNumber(anyList, list3_copy))
println(occurrencesNumber(anyList, 5))
println(occurrencesNumber(anyList, name1))
println(occurrencesNumber(anyList, anyList))

//task4
println(fib(-1))
println(fib(5))
println(fib(20))