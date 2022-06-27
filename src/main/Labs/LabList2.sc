//task 1
def squeeze[A, B](list : List[A], func : (A, A) => B): List[B] = list match {
  case a if a.size < 2 => List()
  case h :: t => func(h, t.head) :: squeeze(t, func)
}

val mean = (a : Double, b : Double) => (a + b) / 2
squeeze(List(1.0, 2.0, 3.0), mean)
squeeze(List(1.0, 2.0, 3.0, 4.0, -1.0), mean)
squeeze(List(1.0), mean)//because there's only one element, nothing to combine with
squeeze(List(), mean)

val whoIsAPair = (name1 : String, name2 : String) => if(name1.charAt(0) == name2.charAt(0)) name1 + " match " + name2 else name1 + " doesn't match " + name2
squeeze(List("Daniel", "Daniella", "Illia", "Andrew", "Anna"), whoIsAPair)

//task 2
def flatten[A](list : List[List[A]]): List[A] = {
  def flattenTailRec(list : List[List[A]], accumulator : List[A]): List[A] = list match {
    case Nil => accumulator
    case h :: t => flattenTailRec(t, accumulator ++ h)
  }
  flattenTailRec(list, List())
}

flatten(List(List(1, 2), List(3, 4)))
flatten(List(List(List(" :) "), List(" :( ")), List(List(" :/ "), List(" :| "))))
flatten(List(List(List()), List(List(List()))))
flatten(List(List(), List()))
flatten(List(List(List(1, 2)), List(List(3, 4))))
