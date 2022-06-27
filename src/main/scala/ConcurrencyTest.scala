import java.util.concurrent.Semaphore
import scala.util.Random
import java.lang.InterruptedException


//task 1
class Buffer(var buffer: Option[Int]) {
  val generator = new Generator(this)

  def getGenerator(): Generator = generator

  def works(): Boolean = generator.isAlive()

  def setNumber(num: Int) = {
    buffer match {
      case None => buffer = Some(num)
      case _ =>
    }
  }

  def getNumber(): Option[Int] = {
    buffer match {
      case Some(a) => buffer = None; Some(a);
      case None => None
    }
  }
}

class Generator(val buffer: Buffer) extends Thread {
  override def run(): Unit = {
    for (i <- 0 until 1000) {
      val temp = Generator.rand.nextInt(4000) + 1000
      println("Putting " + temp + " into buffer;")
      buffer.setNumber(temp)
    }
  }
}

object Generator {
  val rand = new Random()
}

class Reader(val buffer: Buffer) extends Thread {
  override def run(): Unit = {
    while (buffer.works()) {
      println("Taking " + buffer.getNumber() + " from buffer;")
    }
  }
}

//task 2
class Buffer2(var buffer: Option[Int]) {
  val generator = new Generator2(this)

  def getGenerator(): Generator2 = generator

  def works(): Boolean = generator.isAlive()

  def setNumber(num: Int) = {
    var check = true
    while (check) {
      Buffer2.sem.acquire()
      buffer match {
        case None => buffer = Some(num); Buffer2.sem.release(); check = false
        case _ => Buffer2.sem.release();
      }
    }
  }

  def getNumber(): Int = {
    var temp = 0
    var check = true
    while (check && works) {
      Buffer2.sem.acquire()
      if (buffer != None) {
        buffer match {
          case Some(a) => temp = a; buffer = None; check = false
        }
      }
      Buffer2.sem.release()
    }
    temp
  }
}

object Buffer2 {
  val sem = new Semaphore(1)
}


class Generator2(val buffer: Buffer2) extends Thread {
  override def run(): Unit = {
    for (i <- 0 until 1000) {
      val temp = Generator.rand.nextInt(4000) + 1000
      println("Putting " + temp + " into buffer;")
      buffer.setNumber(temp)
    }
  }
}

object Generator2 {
  val rand = new Random()
}

class Reader2(val buffer: Buffer2) extends Thread {
  override def run(): Unit = {
    while (buffer.works()) {
      println("Taking " + buffer.getNumber() + " from buffer;")
    }
  }
}

//Task3
class Buffer3(var buffer: Option[Int]) {
  val generator = new Generator3(this)

  def getGenerator(): Generator3 = generator

  def works(): Boolean = this.synchronized{ generator.isAlive() || buffer != None}

  def setNumber(num: Int) = this.synchronized {
    while (buffer != None) {
      wait()
    }

    buffer = Some(num)
    if (buffer != None) {
      notifyAll()
    }
  }

  def getNumber(): Int = this.synchronized {
    while (buffer == None) {
      wait()
      if(!works()){
        notifyAll()
        throw new InterruptedException
      }
    }
    val temp = buffer
    if (temp != None) {
      notifyAll()
      buffer = None
    }
      temp match {
        case Some(a) => a
        case _ => 0
      }
  }
}


class Generator3(val buffer: Buffer3) extends Thread {
  override def run(): Unit = {
    for (i <- 0 until 1000) {
      val temp = Generator.rand.nextInt(4000) + 1000
      println(i + " Putting " + temp + " into buffer;")
      buffer.setNumber(temp)
    }
  }
}

object Generator3 {
  val rand = new Random()
}

class Reader3(val buffer: Buffer3) extends Thread {
  override def run(): Unit = {
    try {
      while (buffer.works()) {
        println("Taking " + buffer.getNumber() + " from buffer;")
      }
    }
    catch{
      case ex: InterruptedException => this.interrupt()
    }
  }
}

object ConcurrencyTest {
  //test 1
  def main(args: Array[String]): Unit = {
    val buffer = new Buffer(None)
    val gen = buffer.getGenerator()
    val reader1 = new Reader(buffer)
    val reader2 = new Reader(buffer)

    reader1.start()
    reader2.start()
    gen.start()

    gen.join()
    reader1.join()
    reader2.join()

    //task 2
    println("\n\n\n\nTask 2")
    val buffer2 = new Buffer2(None)
    val gen2 = buffer2.getGenerator()
    val reader12 = new Reader2(buffer2)
    val reader22 = new Reader2(buffer2)

    gen2.start()
    reader12.start()
    reader22.start()

    gen2.join()
    reader12.join()
    reader22.join()

    //task3
    println("\n\n\n\nTask 3")
    val buffer3 = new Buffer3(None)
    val gen3 = buffer3.getGenerator()
    val reader13 = new Reader3(buffer3)
    val reader23 = new Reader3(buffer3)

    gen3.start()
    reader13.start()
    reader23.start()

    gen3.join()
    reader13.join()
    reader23.join()
  }
}