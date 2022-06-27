import java.util.{Collections, List}
trait Animal{
  def voice(): Unit ={}
}

class Dog(name: String, sound: String) extends Animal{
  override def voice(): Unit = {
    print(name + ": " + sound)
  }
}
object Dog {
  private val _instance = new Dog("lessie", "bark bark")
  def instance(): Dog = _instance
}

//val hubert = new Dog("Hubert", "bark")
//hubert.voice()

object Bee extends Animal{
  override def voice(): Unit = {
    print("Bzzzz")
  }
}

val dog = Dog.instance()
dog.voice()

class SumNumbers{
  //some implementation
}
object SumNumbers{
  def SumNumbers(list: java.util.Collection[Int]): Int = {
    if(list.size() == 0){
      0
    }
    else{
      val temp = list.toArray()[Int](0)
      return temp
    }
  }
}

SumNumbers.SumNumbers(List(1, 2 ,3))
SumNumbers.SumNumbers(List(6, 1, -3 ,8, 9))

var col = new Collections(1, 2, 4)[Int]
col.toString