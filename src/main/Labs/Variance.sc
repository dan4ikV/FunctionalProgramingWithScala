// ----- INTRODUCTION -----
// This example shows usage of type variance declarations in Scala.
// Let: T' <: T - T' is a subtype of T (T' has interface compatible with T)
//      A[T] - generic class (class parametrized with a type T)
// Then:
//  - A is invariant     if A[T'] and A[T] are unrelated,
//  - A is covariant     if A[T'] <: A[T],
//  - A is contravariant if A[T] <: A[T'].

// If T' is a subtype of T, then objects of T' can be used in places where one would expect objects of type T.
// Invariance     happens when generic class is both: SOURCE of and DESTINATION for data.
// Covariance     happens when generic class is a SOURCE of the data.
// Contravariance happens when generic class is a DESTINATION of the data.

// ----- EXAMPLE -----
// This example shows application of type variance for ZOO problem.
//

// Animal represents a concept of an animal that can have name and makes a sound.
// Both of these properties are immutable.

abstract class Animal(val name: String, val sound: String) {
}

// There are three types of animals in the ZOO - Elephants, Foxes, and Lions.
// New ones could be added.

class Elephant(name: String) extends Animal(name, "Trumpet!")
class Fox(name: String) extends Animal(name, "Ring-ding-ding-ding-dingeringeding!")
class Lion(name: String) extends Animal(name, "Roar!")

// A cage is a container for an animal.
// It is invariant with respect to the type of animal because a cage, such as terrarium, is designed for specific type of an animal.
// Moreover, a cage can both "store" and "load" animals, so it is a SOURCE and a DESTINATION for them.

class Cage[T] {
  var animal: T = _
}

// A checklist is checked by a ZOO keeper.
// It is covariant with respect to its type, because it is read-only - it is a SOURCE for data.

trait Checklist[+T]
case object Empty extends Checklist[Nothing]
case class Entry[A](item: A, rest: Checklist[A]) extends Checklist[A]

// A keeper checks a checklist.

class ZOOKeeper {

  def checkTheList(list: Checklist[Animal]): Unit =
  {
    list match
    {
      case Empty => ()
      case Entry(animal, rest) => {
        println(animal.name + " - CHECKED!")
        checkTheList(rest)
      }
    }
  }
}

// Sound Recorder is a device that can record animal's sound.
// It is contravariant with respect to the type, because it only reads a data - it is a DESTINATION for the data.
// There are two types of sound recorders defined: one that can record sound of any animal, and one designed specifically for fox sounds.

abstract class SoundRecorder[-T] {
  def record(source: T): Unit
}

class AnimalRecorder extends SoundRecorder[Animal] {
  def record(animal: Animal): Unit =
  {
    println("Animal makes sound: " + animal.sound)
  }
}

class FoxRecorder extends SoundRecorder[Fox] {
  def record(fox: Fox): Unit =
  {
    println("What does the fox say?")
    println(fox.sound)
  }
}

// Fox Sounds Fan is a person that loves sounds of foxes and records them.
// It uses a SoundRecorder[Fox] for doing so.
// NOTE: The type used is SoundRecorder[Fox], and not FoxRecorder, because contravariance can be declared for a generic class.
//       FoxRecorder is a subtype of SoundRecorder[Fox], but it cannot be used here - there is no type parameter for the contravariance.

class FoxSoundsFan {
  def recordFoxsSounds(fox: Fox, recorder: SoundRecorder[Fox]): Unit =
  {
    recorder.record(fox)
  }
}

// Three animals are created there.

val mike = new Elephant("Mike")
val sly = new Fox("Sly")
val leon = new Lion("Leon")

// A pit is created - a cage for a lion.
var pit = new Cage[Lion]

// A lion is stored in the pit - it works.

println()
pit.animal = leon
println("Animal in the pit: " + pit.animal.name)

// We try to store a for in the pit - type mismatch error.

println()
pit.animal = sly
println("Animal in the pit: " + pit.animal.name)
println()

// We create a ZOO keeper and two checklists - one of all types of animals and one of elephants.

val keeper = new ZOOKeeper

val animalCheckList: Checklist[Animal] = Entry(mike,
  Entry(sly,
    Entry(leon,
      Empty
    )
  )
)

val elephantCheckList: Checklist[Elephant] = Entry(mike,
  Entry(new Elephant("Steve"),
    Entry(new Elephant("Bob"),
      Empty
    )
  )
)

// Keeper checks the animal list - it works.

println()
println("Checking animal list")
keeper.checkTheList(animalCheckList)

// Then elephant list is checked - it works as well.
// We expected that, because a person that can check list of any animal can do so for a specific type of animals as well.

println()
println("Checking elephant list")
keeper.checkTheList(elephantCheckList)
println()

// We create a Fox Sounds Fan and two recorders.

val foxFan = new FoxSoundsFan

val foxRecorder: SoundRecorder[Fox] = new FoxRecorder
val animalRecorder: SoundRecorder[Animal] = new AnimalRecorder

// Then the fan uses designed recorder for recording sounds of a fox - it works as expected.

println()
println("Recording sound with a fox recorder" )
foxFan.recordFoxsSounds(sly, foxRecorder)

// But if designed recorder can record a sound then general one should do so as well.
// Fan records records the sounds using general Animal Recorder.

println()
println("Recording sound with animal recorder")
foxFan.recordFoxsSounds(sly, animalRecorder)