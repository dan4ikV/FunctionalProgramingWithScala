//task1 duck typing
def makeNoise(animal: {def makeNoise(): String}): Unit ={
  println(animal.makeNoise())
}

class Dog {
  def makeNoise(): String = "bark"
}
class Cat {
  def makeNoise(): String = "meow"
}

makeNoise(new Dog)
makeNoise(new Cat)

//task2
class Foo{
  def method(input: String) = input
}
class Bar{
  def method(input: String) = input
}

implicit def fooToBar(foo: Foo): Bar = new Bar
implicit def barToFoo(bar: Bar): Foo = new Foo

val bar: Bar = new Foo
val foo: Foo = new Bar

//task3
case class Person(firstName: String, lastName: String) extends Ordered[Person]{
  override def compare(that: Person): Int = {
    if(this.firstName.compareTo(that.firstName) != 0){
      this.firstName.compareTo(that.firstName)
    }
    else{
      this.lastName.compareTo(that.lastName)
    }
  }
  def getFirstName(): String = firstName
  def getLastName(): String = lastName
}

val list = List(Person("Danylo", "Vasylyshyn"), Person("Illia", "r"), Person("Illia", "Nykon"), Person("Illia", "Nykonchuk"), Person("Andriy", "Vergun"), Person("Sergiy", "Vergun"))
val other = list.sorted(Ordering[Person].reverse)

//task4
def splitOnWords(text: String): Map[String, Int] = {
  val arr = text.split("\\s*(=>|,|\\s)\\s*")
  var map = Map[String, Int]()
  for(word <- arr){
    if(map.contains(word)){
      var temp = map(word)
      temp = temp + 1
      map += word -> temp
    }
    else{
      map += word -> 1
    }
  }
  map - ""
}

splitOnWords("a b c d e f g a b c, a").toList.toString()
splitOnWords("asdasdasd").toList.toString()
splitOnWords("").toList.toString()
splitOnWords("a,v,s,v, ,a,s,s,d,d").toList.toString()