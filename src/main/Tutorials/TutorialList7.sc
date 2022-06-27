case class Entry[A, B](key: A, value: B){
  def getKey(): A ={
  key
  }
  def getValue(): B ={
    value
  }
}
case class Map[A, B](entryList: List[Entry[A, B]])

def put[A, B](key: A, value : B, map : Map[A, B]): Map[A, B] = {
    def runPut(key: A, value: B, list: List[Entry[A, B]]): List[Entry[A, B]] = list match {
      case Nil => List(Entry(key, value))
      case h :: tail => if(h.getKey() == key) List(Entry(key, value)) ::: tail else h :: runPut(key, value, tail)
    }
    Map(runPut(key, value, map.entryList))
}

def getByKey[A, B](key : A, map: Map[A, B]): Option[B] ={
  def runGet(key: A,list: List[Entry[A, B]]): Option[B] = list match{
    case Nil => None
    case h :: tail => if(h.getKey() == key) Some(h.getValue()) else runGet(key, tail)
  }
  runGet(key, map.entryList)
}

def size[A, B](map : Map[A, B]): Int = {
  map.entryList.size
}

def remove[A, B](key: A, map : Map[A, B]): (Map[A, B], Option[B]) = {
  def runRemove(key: A, list: List[Entry[A, B]]): (List[Entry[A, B]], Option[B]) = list match{
    case Nil => (List(), None)
    case h :: tail => if(h.getKey() == key){ (tail, Some(h.getValue())) }
    else{ val temp = runRemove(key, tail); (h :: temp._1, temp._2) }
  }
  val listAndElement = runRemove(key, map.entryList)
  (Map(listAndElement._1), listAndElement._2)
}

def isEmpty[A, B](map : Map[A, B]): Boolean = {
  map.entryList.isEmpty
}

def printMap[A, B](map : Map[A, B]):Unit = {
  def runPrintMap(list: List[Entry[A, B]]): Unit = list match{
    case Nil =>
    case h :: tail => print(h.getKey() + " " + h.getValue() + ", ")
  }
  print("{ ")
  runPrintMap(map.entryList)
  print(" }\n")
}

var map = Map[Int, String](List())
printMap(map)
isEmpty(map)
map = put(1, "Danylo", map)
printMap(map)
print(getByKey(1, map))
map = put(2, "illia", map)
map = remove(1, map)._1
printMap(map)
print(getByKey(2, map))




//task2
case class Set[A](arr : List[A])

def contains[A, B](set : Set[A], element: B): Boolean = {
  def runContains(list: List[A], element: B): Boolean = list match {
    case Nil => false
    case h :: t => if(h == element) true else runContains(t, element)
  }
  runContains(set.arr, element)
}

def add[A](set : Set[A], element: A): Set[A] ={
  if(!contains(set, element)){
    Set(set.arr.appended(element))
  }
  else{
    set
  }
}

def remove[A](set: Set[A], element: A): (Set[A], Option[A]) = {
  def runRemove(list: List[A]): (Set[A], Option[A]) = list match {
    case Nil => (Set(List[A]()), None)
    case h :: t => if(h == element){ (runRemove(t)._1, Some(h)) }
    else{val temp = runRemove(t); (add(temp._1, h), temp._2)}
  }
  runRemove(set.arr)
}

def intersection[A](set1: Set[A], set2: Set[A]): Set[A] = {
  def runIntersection(list: List[A]): Set[A] = list match {
    case Nil => Set(List[A]())
    case h :: t => if(contains(set2, h)) add(runIntersection(t), h) else runIntersection(t)
  }
  runIntersection(set1.arr)
}

def union[A](set1: Set[A], set2: Set[A]): Set[A] = {
  val commonSet = intersection(set1, set2)
  def runUnion(list: List[A]): Set[A] = list match {
    case Nil => set2
    case h :: t => if(contains(commonSet, h)) runUnion(t) else add(runUnion(t), h)
  }
  runUnion(set1.arr)
}

def difference[A](set1: Set[A], set2: Set[A]): Set[A] = {
  def runDifference(list: List[A], tempSet: Set[A]): Set[A] = list match {
    case Nil => Set(List[A]())
    case h :: t => if(!contains(tempSet, h)) add(runDifference(t, tempSet), h) else runDifference(t, tempSet)
  }
  union(runDifference(set1.arr, set2), runDifference(set2.arr, set1))
}

def printSet[A](set: Set[A]): Unit = {
  def runPrintSet[A](list: List[A]): Unit = list match {
    case Nil =>
    case h :: t => { print(h + ", "); runPrintSet(t) }
  }
  print("{ ")
  runPrintSet(set.arr)
  print("}\n")
}

var set = Set(List[Int]())
printSet(set)
set = add(set, 1)
set = add(set, 2)
set = add(set, 3)
set = add(set, 4)
set = add(set, 5)
printSet(set)
var set2 = Set(List[Int]())
set2 = add(set2, 3)
set2 = add(set2, 4)
set2 = add(set2, 5)
set2 = add(set2, 6)
set2 = add(set2, 7)
printSet(set2)
printSet(intersection(set, set2))
printSet(union(set, set2))
printSet(difference(set, set2))
set2 = remove(set2, 3)._1
printSet(set2)
printSet(intersection(set, set2))
printSet(union(set, set2))
printSet(difference(set, set2))

