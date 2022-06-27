//task 1
//functional
def repeat[A](listOfValues: List[A], listOfRepetitions: List[Int]): List[A] = {
  def goRepeat(listOfValues: List[A], listOfRepetitions: List[Int], accumulator: List[A]): List[A] = (listOfValues, listOfRepetitions) match{
    case (h1 :: t1, h2 :: t2) => goRepeat(t1, t2, accumulator ::: listOf(h2 , h1))
    case _ => accumulator
  }
  def listOf(num : Int, value : A): List[A] = num match {
    case 0 => List()
    case _ => value :: listOf(num - 1, value)
  }
  goRepeat(listOfValues, listOfRepetitions, List())
}

val list1 = List(1, 2, 3, 4, 5)
val list2 = list1
repeat(list1, list1)
val empty = List()
repeat(list1, empty)
repeat(empty, list1)
val face = List(")", ":", ")")
var sad =  List(1, 1)
var happy =  List(0, 1, 1)
var bipolar =  List(1, 1, 1)
repeat(face, sad).mkString
repeat(face, happy).mkString
repeat(face, bipolar).mkString

//imperative
def repeat[A: Manifest](arrayOfValues: Array[A], arrayOfRepetitions: Array[Int]): Array[A] = {
  val minSize = math.min(arrayOfRepetitions.size, arrayOfValues.size)
  var arr = new Array[A](0)
  var index = 0
  for(i <- 0 until minSize){
    for(j <- 0 until arrayOfRepetitions(i)){
      arr = arr :+ arrayOfValues(i)
      index += 1;
    }
  }
  arr
}

val list11 = Array(1, 2, 3, 4, 5)
val list22 = list11
repeat(list11, list11)
val empty1 = Array[Int]()
repeat(list11, empty1)
repeat(empty1, list11)
val face1 = Array(")", ":", ")")
var sad1 =  Array(1, 1)
var happy1 =  Array(0, 1, 1)
var bipolar1 =  Array(1, 1, 1)
repeat(face1, sad1).mkString
repeat(face1, happy1).mkString
repeat(face1, bipolar1).mkString

//task 2

class BinarySearchTree(value: Int, var left: BinarySearchTree, var right: BinarySearchTree){
  def this(value: Int){
    this(value, null, null)
  }

  def add(value: Int): Unit = value match {
    case a if a < this.value => if(left == null) left = new BinarySearchTree(value) else left.add(value)
    case a if a > this.value => if(right == null) right = new BinarySearchTree(value) else right.add(value)
    case _ =>
  }

  def inOrderPrint(): Unit = {
    if(left != null){
      left.inOrderPrint()
    }
    print(value + ", ")
    if(right != null){
      right.inOrderPrint()
    }
  }
}



var tree = new BinarySearchTree(1)
tree.add(2)
tree.add(3)
tree.inOrderPrint()

