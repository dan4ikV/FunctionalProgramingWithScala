//Task2_a
class Person(name: String, age: Int, sex: Boolean)

class Employee(name: String, age: Int, sex: Boolean, uniqueId: Int, salary: Double) extends Person(name, age, sex) {
  def this(name: String, age: Int, sex: Boolean, salary: Int) {
    this(name, age, sex, Employee.getId(), salary)
  }

  def info(): Unit = {
    println("name: " + name + " ,  age: " + age + " , sex: ");
    if (sex == false) {
      print("male")
    }
    else {
      print("female")
    }
    print(" , unique id: " + uniqueId)
  }

  def getAge(): Int = age
  def getSex(): Boolean = sex
  def getName(): String = name
  def getSalary(): Double = salary
  def getId(): Int = uniqueId
}

object Employee {
  var uniqueId = 0

  def getId(): Int = {
    uniqueId = uniqueId + 1;
    uniqueId
  }
}

val employee1 = new Employee("Danylo", 18, false, 400)
val employee2 = new Employee("Illia", 19, false, 300)
employee1.info()
employee2.info()
val employee3 = new Employee("Jessica", 19, true, 350)
employee3.info()

//Task2_b
trait Accountant {
  def calculateBonus(employee: Employee): Double = {
    employee.getSalary() * (employee.getAge() - 18) / 100
  }
}

val accountant = new Employee("Grzegorz", 25, false, 20) with Accountant
accountant.calculateBonus(employee1)
accountant.calculateBonus(employee2)
accountant.calculateBonus(employee3)

//Task3_a
trait Toy
class Car(name: String, model: Int, colour : String) extends Toy {
  override def toString: String = {
    "Car: " + "\n " + "name: " + name + " , colour: " + colour + " , model: " + model + "\n"
  }
}

class Truck(name: String, model: Int, colour : String) extends Car(name, model, colour) {
  override def toString: String = {
    "Truck: " + "\n " + "name: " + name + " , colour: " + colour + " , model: " + model + "\n"
  }
}

class Doll(name: String, sex : Boolean) extends Toy {
  override def toString: String = {
    var buffer = "Doll: " + "\nname: " + name + " , sex: "
    if(sex == false){
      buffer = buffer + "male"
    }
    else{
      buffer = buffer + "female"
    }
    buffer + "\n"
  }
}

//Task3_b
trait Elf[+T <: Toy]{
  def makeToy(): T
}

class ElfCarMaker() extends Elf[Car]{
  override def makeToy(): Car = {
    var name = ""
    var colour = ""
    var rand = scala.util.Random.nextInt(3)
    if(rand == 0) name = "BMW" else if (rand == 1) name = "Mercedes" else if (rand == 2) name = "Toyota"
    rand = scala.util.Random.nextInt(3)
    if(rand == 0) colour = "red" else if (rand == 1) colour = "green" else if (rand == 2) colour = "blue"
    new Car(name, scala.util.Random.nextInt(100), colour)
  }
}

class ElfTruckMaker() extends Elf[Truck]{
  override def makeToy(): Truck = {
    var name = ""
    var colour = ""
    var rand = scala.util.Random.nextInt(3)
    if(rand == 0) name = "BMW" else if (rand == 1) name = "Mercedes" else if (rand == 2) name = "Toyota"
    rand = scala.util.Random.nextInt(3)
    if(rand == 0) colour = "red" else if (rand == 1) colour = "green" else if (rand == 2) colour = "blue"

    new Truck(name, scala.util.Random.nextInt(100), colour)
  }
}

class ElfDollMaker() extends Elf[Doll]{
  override def makeToy(): Doll = {
    if(scala.util.Random.nextBoolean() == false){
      var name = ""
      val rand = scala.util.Random.nextInt(5)
      if(rand == 0) name = "John" else if (rand == 1) name = "Bob" else if (rand == 2) name = "Ken" else if(rand == 3) name = "Danylo" else if(rand == 4) name = "Jakob"
      new Doll(name, false)
    }
    else{
      var name = ""
      val rand = scala.util.Random.nextInt(5)
      if(rand == 0) name = "Mia" else if (rand == 1) name = "Jessica" else if (rand == 2) name = "Lia" else if(rand == 3) name = "Anna" else if(rand == 4) name = "Kayla"
      new Doll(name, true)
    }
  }
}

val carMaker = new ElfCarMaker
carMaker.makeToy()
carMaker.makeToy()
carMaker.makeToy()

val dollMaker = new ElfDollMaker
dollMaker.makeToy()
dollMaker.makeToy()
dollMaker.makeToy()
dollMaker.makeToy()

class Workshop(var elves : List[Elf[Toy]]){
  def this(){this(List[Elf[Toy]]())}
  def employ(elf: Elf[Toy]): Unit = elves = elves.appended(elf)
  def makeToys(): List[Toy] = {
    var toys = List[Toy]()
    for(elf <- elves){
      toys = toys.appended(elf.makeToy())
    }
    toys
  }
}

val workshop = new Workshop()
workshop.employ(carMaker)
workshop.employ(dollMaker)
workshop.makeToys().toString()