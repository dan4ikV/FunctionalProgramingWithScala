//Task 1
class Animal(name: String)
class Dog(name: String, breed: String) extends Animal(name)

def checkCovariance(arr: Array[Animal]):Unit={}
def checkContravariance(arr: Array[Dog]):Unit={}

val animalArray = new Array[Animal](1)
val dogArray = new Array[Dog](1)

//checkCovariance(dogArray)
//checkContravariance(animalArray)
//in scala arrays are invariant because the relation is neither Array[Dog] <: Array[Animal] nor Array[Animal] <: Array[Dog]

//Task2
//functional
def removeRepetitions[T](list: List[T]): List[T] = {
  def removeRepetitionsWithAccumulator(list: List[T], accumulator: List[T]): List[T] = list match {
    case Nil => List[T]()
    case h :: t => if(accumulator.contains(h)) removeRepetitionsWithAccumulator(t, accumulator)
    else h :: removeRepetitionsWithAccumulator(t, h :: accumulator)
  }
  removeRepetitionsWithAccumulator(list, List[T]())
}

removeRepetitions(List(1, 2, 3, 4, 5, 5, 4))
removeRepetitions(List())
removeRepetitions(List(List(1, 2), List(2, 3), List(1, 2), List(2, 3)))

//imperative
def removeRepetitions[T: Manifest](array: Array[T]): Array[T] = {
  var newArray = Array[T]()
  for(i <- 0 until array.length){
    if(!contain(newArray, array(i))){
      newArray = newArray :+ array(i)
    }
  }
  def contain(array: Array[T], value: T): Boolean = {
    var contain = false
    for(i <- 0 until array.length){
      if(array(i) == value){
        return true
      }
    }
    contain
  }
  newArray
}


removeRepetitions(Array(1, 2, 3, 4, 4, 5))
removeRepetitions(Array("a", "b", "c", "d", "a", "a"))
removeRepetitions(Array[Int]())