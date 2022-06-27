
// Inheritance in Scala

abstract class Animal(val name: String){
  def giveVoice: Unit // = println("Angry animal noises") instead of abstract (default implementation)
}

class Dog(name: String) extends Animal(name){
  override def giveVoice: Unit = println(name + ": *bark!* *bark!*")
}

val mike = new Dog("Mike")
mike.giveVoice

val steve: Animal = new Dog("Steve")
steve.giveVoice             // still barks, even if the type is Animal

// JAVA
/*class Dog extends Animal{
  public Dog(String name){
    super(name)
  }
  // ...
}*/

// Note on case classes and case objects

// recall
sealed trait MyList[+T]
case object MyNil extends MyList[Nothing]
case class MyCons[T](head: T, tail: MyList[T]) extends MyList[T]

// case classes and objects are regular classes/objects
case object MyCaseObject{
  def sum(a: Double, b: Double): Double = a + b

  val myConstant = 44
}

MyCaseObject.sum(10.5, 15.0)
MyCaseObject.myConstant

// Differences between case and non-case classes/objects:
// - case classes/objects have nice toString methods defined
MyCons(10, MyNil)     // notice the representation on the right of the "=" sign
// - case classes/objects can be used in the pattern matching
//   this is achieved by implementation for apply and unapply methods (check the documentation for details)
val MyCons(h, t) = MyCons(10, MyCons(20, MyNil))

// The "case" keyword is a syntactic sugar for doing all of that manually

// Traits and Mix-ins

// Trait is similar to JAVA's interface but the idea behind it is to provide some functionalities that we can add freely to our classes

trait Greetable{      // Trait as an interface
  def greet: Unit
}

trait Calculation{    // Trait with some functionality
  def sum(a: Double, b:Double): Double = a + b
}

class A extends Greetable{
  override def greet: Unit = println("Greetings from class A!")
}

val a = new A
a.greet

class B extends Calculation

val b = new B
b.sum(10, 20)

class C extends Greetable with Calculation{         // The Calculation trait is mixed-in into our C class, this is why we call it a mix-in
  override def greet: Unit = println("Greetings from class C!")
}

val c = new C
c.greet
c.sum(10, 20)

// Notation: A <- B  - this means that A is a superclass of B (B inherits/extends A)
// For mix-ins the class hierarchy is linearized
// In our example: Greetable <- Calculation <- C
// In general if we have:
// class B extends A with M1 with M2 with M3 (and so on)
// we will have: A <- M1 <- M2 <- M3 <- B
// Notice, that mix-ins do not know their base classes at the time of definition

val Bob = new Dog("Bob") with Calculation   // Calculation is mixed-in into a specific instance!
// Notice that type of our Bob object is some new anonymous class
Bob.giveVoice
Bob.sum(27.555, -10.3)

// Abstract classes vs traits:
// - traits cannot have constructors,
// - abstract classes cannot be mixed-in
// - both can be used as a base class
// - abstract classes have to know their base classes at the definition time
//   traits will "acknowledge" their base class at the use time

// Parametric polymorphism (generic classes)

class Cell[T]{
  var value: T = _
}
object Cell{
  def apply[T](value: T): Cell[T] = {
    val cell = new Cell[T]
    cell.value = value
    cell
  }
}

val c1 = Cell(10)
val c2 = Cell("Hello")

c1.value
c2.value

val cD: Cell[Dog] = Cell(new Dog("Jenny"))
//val cA: Cell[Animal] = cD

// Why cannot we assign a cell of Dog to a cell of Animal if Dog IS an Animal?

// Answer to that: type variance!

// Type variance

// Recall: TYPE = VALUES + OPERATION

// Types naturally create hierarchies - we create new kinds of values of existing type or we add new operations to some subset of type's values

// There are two types of hierarchies:
// - inheritance (reusage of implementation),
// - subtyping (interface-related hierarchy of types).
// These types of hierarchies can overlap or be separate (in general)

// Inheritance + subtyping - regular class extension in JAVA or Scala

// Inheritance without subtyping - in C++ it can be achieved with protected/private inheritance
// e.g. class B : private A { /*...*/ };

// Subtyping without inheritance - in C++ collections in the standard library
// have common interfaces (same names of methods in different classes, e.g. begin(), end(), operator[], size(), ...)
// but notice that vector<T> and queue<T> does NOT inherit from any common super class
// At the same, you can use both of them in standard algorithms

// For two types S and T, S <: T means that S is subtype of T
// Similary, T >: S means that T is a supertype of S

// When type S is a subtype of type T (S <: T) every value
// of the S type can be used in place where a value of type T is expected
// Subtyping relation can be compared to the subset relation between sets
// Indeed, if S <: T then set of values of S IS a subset of values of T
// Notice, that a type is it's own subtype (S <: S)

// Type variance problem

// Imagine that we have class Stack[T] that can contain any type of elements
// If S <: T what will be the relationship between Stack[T] and Stack[S]?

// Invariance
// By default if S <: T then Stack[S] and Stack[T] are independent
// this is the case of Dog <: Animal and Cell[Dog] and Cell[Animal]

// Covariance
// if S <: T then Stack[S] <: Stack[T]      // Notice that directions of "<:" match
// In covariant Stack: Stack[Dog] <: Stack[Animal]

// Covariant datatypes can be treated as sources of data
// Imagine that we are generating Animals in Generator[Animal] class
// Because it generates data we can use Generator[Dog] in it's place
// because set of values of Dog datatype is a subset ov values fo Animal datatype

// When we want to tell that our datatype is covariant with respect to selected
// type T we add + in front of this T, e.g. sealed trait MyList[+T]

// Contravariance
// if S <: T then Stack[T] <: Stack[S]      // Notice that directions of "<:" is reversed
// In contravariant Stack: Stack[Animal] <: Stack[Dog]

// Contravariant datatypes can be treated as "drains"/"destinantions" for data
// Imagine that we are having boxes in which we can store Dogs e.g Box[Dog] class
// Because it can accomodate Dogs only it cannot accomodate other animals
// at the same time Box[Animal] can store any animal, so it can store Dogs as well

// When we want to tell that our datatype is contravariant with respect to selected
// type T we add - in front of this T, e.g. sealed trait Box[-T]

// Example given in variance.scala file

// Function types subtyping

// As we already know, in functional programming languages
// functions have their own types eg. Int => Float, Person => Age,
// Int => Int => Int, (Double => Double) => Double => Double, ect.
// A specific function of given type (with information about values
// of variables defined in its environment) is called a closure.

// If our functions have types, can we create a hierarchy of them?
// In other words, if we expect a function of type S => T,
// what are the requirements for S' => T' function type
// to be a subtype of S => T  (S' => T' <: s => T)?

// Notice, that a function can be analysed from two ends:
// - its argument (remember that in functional programming languages functions
//                 can have only one argument - Scala breaks this requirement),
// - its output.

// Note: In Scala types such as: Double, Int, Byte are NOT in a relation (Int <: Double IS NOT true).
//       For clarity, in the following example WE WILL ASSUME that Byte <: Int <: Double.

// We do expect a function of type Int => Int

// Let's focus on the argument.
// We know that our function expects integers as its inputs,
// so the function that we would liked to put in its place
// have to accept AT LEAST integers.
// As a result our substitution can have Int or Double
// as the type of the argument, but not the Byte (because Double
// covers all of the possible integers, but byte does not).
// Notice, that for argument we can use any SUPERTYPE of original
// input type.

// Let's move to the output.
// Our function is expected to generate integers.
// If we wanted to use a function that generates Doubles,
// in place of our original function, we would have an error
// because the place in which our function will be used expects
// integers only.
// On the other hand, there would be no problem if our substitution
// generated only a small subset of all integers, because
// these values still would be in the expected range.
// As a result, we could use any function that has Int or Byte
// as a type of its output.
// Notice, that for output we can use any SUBTYPE of original
// output type.

// Now, let's combine our findings.

// We expected a function of type Int => Int

// From the first analysis we know that we can use Int => ? or Double => ?
// functions in its place
// Similarly, from the second analysis we know that ? => Int or ? => Byte
// functions are correct in the substitution.

// From both of this points we can conclude that functions of types:
// - Int => Int
// - Double => Int
// - Int => Byte
// - Double => Byte
// can be used in place where Int => Int function was expected.
// In other words, all of the above are SUBTYPES of type Int => Int

// In this example the following hierarchy can be deduced:
//            Int => Int
// Double => Int      Int => Byte
//         Double => Byte
// (The highest supertype is Int => Int, the lowest subtype is Double => Byte)

// What is the general rule?
// Given two function types: S => T and S' => T'
// (S' => T') <: (S => T) if and only if S <: S' and T' <: T.
// Which means that function types are:
// - CONTRAVARIANT with respect to their arguments,
// - COVARIANT with respect to their results.
// Of course, it agrees with our previous observation
// that data drains are contravariant and data sources are covariant.
// Indeed, function's argument IS a drain for data and
// function's output is a source of data.

// In Scala, functions are represented by generic class FunctionN
// with method apply defined (N - is the number of arguments).
// For one argument functions the class Function1 has the following type:
// trait Function1[-T, +R] extends AnyRef
// with abstract method: abstract def apply(arg1: T): R

// following definitions are equivalent
// the first one is just a syntactic sugar for the latter one
val next = (x:Int) => x + 1
val nextF = new Function1[Int, Int] {   // here we have unnamed class implementing Function1 trait
  def apply(x: Int): Int = x + 1
}

next(10)
nextF(10)

// Bounded polymorphism

// Polymorphism is a property meaning that given part
// of a program is type independent.
// It means that it can be used with different types.
// So far we have discussed two classes of polymorphism:
// - parametric polymorphism - creation of classes and functions that can be parametrized with different types,
// - inclusion polymorphism / subtype polymorphism - subtypes can be used in places where supertype is expected.

// Bounded polymorphism is a combination of both parametric and inclusion polymorphism
// which allows us to specify constraints on the type parameter of our class/function.
// These constraints can be either the minimal supertype (upper type bound)
// or maximal subtype (lower type bound).

// In JAVA, bounded polymorphism can be used
// with super and extends keywords:
// public class <T extends Person> House { /*...*/ } // minimal supertype declaration
// public class <T super Employee> Workplace { /*...*/ } // maximal subtype declaration

// In Scala, bounded polymorphism can be used at the place of definition.
// Notation <: and >: is used for the definition of upper and lower bounds.
// The creator of the class decides what types it can support.

// Upper type bounds in Scala

// Let's add a few classes to our Animal hierarchy

class Cat(name: String) extends Animal(name){
  override def giveVoice: Unit = println(name + ": *meooooooow!*")
}

class Bernard(name: String) extends Dog(name){
  override def giveVoice: Unit = println(name + ": *BORK!* *BORK!*")
}

val rex = new Bernard("Rex")
val snowball = new Cat("Snowball")

// Now let's create a Doghouse class

class Doghouse(d: Dog){
  val dog = d
}

val mikeHouse = new Doghouse(mike)
val rexHouse = new Doghouse(rex)
// val snowballHouse = new Doghouse(snowball)  // error: type mismatch;
                                                  // found   : Cat
                                                  // required: Dog

// This class works, but we loose information about dog's breed (specific type)

class BetterDoghouse[T](d: T){
  val dog = d
}

val mikeBetterHouse = new BetterDoghouse(mike)
val rexBetterHouse = new BetterDoghouse(rex)
val snowballBetterHouse = new BetterDoghouse(snowball) // Whoops!
val intBetterHouse = new BetterDoghouse(12) // What?

// Now we know the type, but anything can be stored in the BetterDoghouse

class BestDoghouse[T <: Dog](d: T){ // Now T can only be any subtype of Dog
  val dog = d
}

val mikeBestHouse = new BestDoghouse(mike)
val rexBestHouse = new BestDoghouse(rex)
// val snowballBestHouse = new BestDoghouse(snowball) // error: inferred type arguments [Cat] do not conform to value <local BestDoghouse>'s type parameter bounds [T <: Dog]

// This class not only preserves the type of it's contents, but also limits the allowed type

// Lower type bounds in Scala

// Let's create immutable Stack

class IStack[T] private (private val rep: List[T]) { // primary constructor is private
  def push(x: T): IStack[T] = new IStack(x::rep)
  def top: Option[T] = rep match {
    case x::_ => Some(x)
    case Nil => None
  }
  def pop: IStack[T] = rep match {
    case _::xs => new IStack(xs)
    case Nil => this
  }
  def isEmpty: Boolean = rep == Nil
}
object IStack { // companion object
  def apply[T](xs: T*): IStack[T] = new IStack[T](xs.toList.reverse)
  def empty[T]: IStack[T] = new IStack[T](Nil)
}

// In apply, asterisk after type T of last argument means that it can be repeated zero or more number of times.
// In the function, xs has type Seq[T] (an unspecified sequence of values)

// Now we can use it:

val animals = IStack.empty[Animal]
val animals1 = IStack(mike, rex, snowball) // animals will be pushed in this order
animals1.top // Snowball on top
animals1.pop

// Now, let's make this a covariant stack, so it can be used as a source of data

/*class ICStack[+T] private (private val rep: List[T]) { // notice the + specifier
  def push(x: T): ICStack[T] = new ICStack(x::rep) // error: covariant type T occurs in contravariant position in type T of value x
  def top: Option[T] = rep match {
    case x::_ => Some(x)
    case Nil => None
  }
  def pop: ICStack[T] = rep match {
    case _::xs => new ICStack(xs)
    case Nil => this
  }
  def isEmpty: Boolean = rep == Nil
}
object ICStack { // companion object
  def apply[T](xs: T*): ICStack[T] = new ICStack[T](xs.toList.reverse)
  def empty[T]: ICStack[T] = new ICStack[T](Nil)
}*/

// There's a problem - push method gives an error "covariant type T occurs in contravariant position"
// Why?
// We declared ICStack as covariant with respect to T,
// but we know that functions are contravariant with respect to their arguments
// As a result, type T has to be covariant and contravariant at the same time,
// which results in invariant type.
// This problem can be solved with lower type bound definition, as follows:

class ICStack[+T] private (private val rep: List[T]) { // notice the + specifier
  def push[U >: T](x: U): ICStack[U] = new ICStack(x::rep) // push creates a new stack of type U being a supertype of T
  def top: Option[T] = rep match {
    case x::_ => Some(x)
    case Nil => None
  }
  def pop: ICStack[T] = rep match {
    case _::xs => new ICStack(xs)
    case Nil => this
  }
  def isEmpty: Boolean = rep == Nil
}
object ICStack { // companion object
  def apply[T](xs: T*): ICStack[T] = new ICStack[T](xs.toList.reverse)
  def empty[T]:  ICStack[T] = new ICStack[T](Nil)
}

val animals2 = ICStack(mike, rex, snowball)
val bernards = ICStack.empty[Bernard].push(rex) // Stack of Bernards
val dogs = bernards.push(mike) // Now, we have Stack of Dogs
val animals3 = dogs.push(snowball) // And Stack of Animals

def stackVoices(animals: ICStack[Animal]): Unit = {
  if(!animals.isEmpty){
    animals.top.get.giveVoice
    stackVoices(animals.pop)
  }
}

println("animals2:")
stackVoices(animals2)
println("bernards:")
stackVoices(bernards)
println("dogs:")
stackVoices(dogs)
println("animals3:")
stackVoices(animals3)

// Notice that class ICStack is indeed covariant because
// stackVoices function works for all of the subtypes of Animal class