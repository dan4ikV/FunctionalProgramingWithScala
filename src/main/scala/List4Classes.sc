//task1
def sieve(s: Stream[Int]): Stream[Int] = s.head #:: sieve(s.tail filter(_ % s.head != 0))

def primes(list: List[Int]): List[Int] = {
    val primes = sieve(Stream.from(2))
    val primesList = primes.take(46).toList
    return list.filter(primesList.contains(_))
}

primes(List.range(1, 940))
primes(List.range(66, 100))

//task2
sealed trait Operation
    case class Add(num1 : Double, num2: Double) extends Operation
    case class Negate(num : Double) extends Operation


def calculator(o : Operation): Double = o match {
    case Add(num1,num2) => num1+num2
    case Negate(num) => -num
}

calculator(Add(5, -5))
calculator(Add(5, 5))
calculator(Add(-6, -6))
calculator(Negate(-0.0))
calculator(Negate(-1))
calculator(Negate(1))

//task3
sealed trait Bool
final case class True() extends Bool
final case class False() extends Bool

def And(bool1: Bool, bool2: Bool): Bool = (bool1, bool2) match {
  case (True(), True()) => True()
  case _ => False()
}

def Or(bool1: Bool, bool2: Bool): Bool = (bool1, bool2) match {
  case (False(), False()) => False()
  case _ => True()
}

def Xor(bool1: Bool, bool2: Bool): Bool = (bool1, bool2) match {
  case (True(), True()) => False()
  case (False(), False()) => False()
  case _ => True()
}

def Nand(bool1: Bool, bool2: Bool): Bool = (bool1, bool2) match {
  case (True(), True()) => False()
  case _ => True()
}

def Nor(bool1: Bool, bool2: Bool): Bool = (bool1, bool2) match {
  case (False(), False()) => True()
  case _ => False()
}

And(True(), True())
And(True(), False())
Or(True(), False())
Or(False(), False())
Xor(False(), True())
Xor(True(), True())
Nand(False(), True())
Nand(True(), True())
Nor(False(), False())
Nor(False(), True())

//task4
def whatType[T](thing: T): Unit = thing match {
  case i: Int => print("That's an int")
  case d: Double => print("That's a double")
  case c: Char => print("That's a char")
  case s: String => print("That's a string")
  case f: Float => print("That's a float")
  case l: List[_] => print("A list")
  }

whatType("AAAAAAAAAAAa")
whatType(1.6)
whatType(1)
whatType('c')
whatType(1.2312312431231245)
whatType(List.range(1, -8))

