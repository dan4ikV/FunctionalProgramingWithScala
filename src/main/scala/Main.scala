import java.util.concurrent.Semaphore
import scala.util.Random

// Process vs Thread
// Process - a program that is being executed
// Thread - a part of program that is being executed in parallel
// Processes have separate memory
// Threads use their process's memory

class MyThread(name: String) extends Thread(name){
  override def run(): Unit = {
    println("Hello from " + Thread.currentThread().getName)
  }
}

class Person(val name: String);
class ParallelWorker(name: String) extends Person(name) with Runnable{    // How to run it in parallel?? Make it Runnable!
  override def run(): Unit = {
    println("Hello from " + Thread.currentThread().getName + " also known as " + name)
  }
}

// Looks simple, where's the catch?

class CalculatingWorker(private val fun: Int => Int) extends Thread{
  override def run(): Unit = {
    CalculatingWorker.x = fun(CalculatingWorker.x)
  }
}
object CalculatingWorker{
  private var x: Int = 0
  def printX = println(x)
}

// Because memory shared between threads was mutated the results of execution were unpredictable.
// The reason for that was that the parts of threads that were going to modify the memory might have been
// interrupted byt the scheduler process that decided to switch the thread currently being executed.
// Combining this with the fact that a thread might have already loaded a value from memory, that was later modified by other thread,
// results with unexpected, erroneous outcomes.
// A part of code that has to be executed without interruptions is called a critical section.
// There is a need for mechanisms that will ensure mutual exclusion.
// Basic two mechanisms are semaphores and monitors.

// Fix1: Semaphores
// Semaphore is a counter that counts how many units/instances of a resource are available

class CalculatingWorkerFix1(private val fun: Int => Int) extends Thread{
  override def run(): Unit = {
    CalculatingWorkerFix1.sem.acquire()       // Ask for access to the critical section (it may block)
    CalculatingWorkerFix1.x = fun(CalculatingWorkerFix1.x)  // the critical section
    CalculatingWorkerFix1.sem.release()       // Release the section
  }
}
object CalculatingWorkerFix1{
  private val sem = new Semaphore(1)    // Notice there is only one Semaphore for all of the threads
  private var x: Int = 0
  def printX = println(x)
}

// Monitor
// Monitor is an object which operations (marked as synchronised) have to be executed as whole
// Monitor can be seen as a binary semaphore that automatically locks synchronised methods (critical sections)
// In JAVA and Scala every object can be used as a Monitor

class CalculatingWorkerFix2(private val fun: Int => Int) extends Thread{
  override def run(): Unit = CalculatingWorkerFix2.x.synchronized {     // x is the monitor used for blocking (In JAVA we would use synchronised keyword)
    CalculatingWorkerFix2.x = fun(CalculatingWorkerFix2.x) // the critical section
  }
}

object CalculatingWorkerFix2{
  private var x: Int = 0
  def printX = println(x)
}

// Thread communication

// Sometimes, a thread that received access to a critical section is unable to finish its job.
// The reason for that may be, for example, unavailability of some resource.
// Additionally, there may be a situation in which a thread loads some data from a database
// and there are other threads waiting for it to finish.

// These examples show that there is a need for some communication mechanism that threads
// can use to inform each others about events that happen.

// Monitors define three methods that can be used for this purpose:
// - wait() - method wait stops execution of the current thread and releases the lock of the monitor.
//            There is an overload which accepts a time for which the thread is being stopped,
// - notify() - method notify wakes up one of threads stopped by wait. The choice of the thread depends on the implementation of JVM.
// - notifyAll() - method notifyAll wakes up ALL of the threads that were waiting for some event. The order in which they will be
//                 woken up is implementation dependent but it is guaranteed that all of the threads will be resumed.

// Generally speaking, the choice whether one should use notify or notifyAll depends on the specific situation.
// It is important to notice that a monitor has only one set of wait-notify methods. As a consequence, threads
// can be informed about only one type of event.
// Of course, there may be a situation in which threads may need a way of reacting to different kinds of events.
// This can be achieved by utilizing locks and conditional variables.
// In JAVA's standard library these are represented by classes found in java.util.concurrent.locks - Lock and Condition.
// This mechanism won't be covered in this lecture (due to lack of time)

// Example: bread production

// Let's model the following situation.
// A few factories bake bread loafs which are delivered to a bakery.
// At the bakery clients are buying the loafs as soon as these arrive.
// Maximal number of loafs that bakery can store is limited (N).

// As we can see the bakery is a kind of container that is used for "communication"
// between factories and clients - this is our shared memory that needs locking when
// data is being written or read.

// for simplicity loafs of bread will be represented by numbers

class Bakery(n: Int, factories: Array[Factory]) {   // We store reference to factories so we know if there will be more deliveries
  private val queue = new scala.collection.mutable.Queue[Int]()  // a queue used to store bread

  def works: Boolean = this.synchronized{
    var jobContinues = false
    for(factory <- factories if !jobContinues){
      jobContinues = factory.isAlive
    }

    jobContinues || !queue.isEmpty
  }

  def getALoaf(): Int = this.synchronized{
    while(queue.isEmpty){ // No bread at store
      wait()  // We wait while there is no bread in bakery.
      // ALWAYS call wait() in a loop! Reason for that is that a thread may be woken up and immediately
      // stopped by the scheduler. It may result in a situation in which another thread steals the stored data
      // from our thread and as a result the collection may STILL be empty. We check that in a loop to prevent the situation.
    }

    val element = queue.dequeue()
    if(queue.size < n){ // Some free space is left in the storage. We perform this check because a factory may have stored
      // a bread after we took one.
      notifyAll()     // This notifies all of the factories that they should try storing bread
    }
    element
  }

  def storeALoaf(x: Int): Unit = this.synchronized{
    while(queue.size >= n){ // No free space left
      wait()
    }

    queue.enqueue(x)
    if(!queue.isEmpty){ // There's some bread stored. We perform this check, because a client could have bought a loaf in the meantime.
      notifyAll()     // Wake up all of the clients
    }
  }

  def print(): Unit = this.synchronized{
    println(queue)
  }
}

class Factory(number:Int, private var breads: List[Int], bakery: Bakery) extends Thread{
  override def run(): Unit = {
    while(breads != Nil) {
      bakery.storeALoaf(breads.head)
      println("Factory " + number + " stores bread " + breads.head)
      bakery.print()
      breads = breads.tail
      Thread.sleep(500 + Factory.random.nextInt(500))  // Thread.sleep() makes a thread go to sleep for given amount of time
    }
  }
}
object Factory{
  private val random = new Random
}

class Client(number: Int, bakery: Bakery) extends Thread{
  override def run(): Unit = {
    while(bakery.works){
      println("Client " + number + " buys bread " + bakery.getALoaf())
      bakery.print()
      Thread.sleep(500 + Client.random.nextInt(500))  // Thread.sleep() makes a thread go to sleep for given amount of time
    }
  }
}

object Client{
  private val random = new Random
}


object Lect5 {
  def firstExample(): Unit ={
    val myThread = new Thread("MyThread")   // By default it does nothing
    myThread.start()
  }

  def secondExample(): Unit ={
    val myThread = new MyThread("John")
    myThread.start()
  }

  def thirdExample(): Unit ={
    val worker = new Thread(new ParallelWorker("Jeff"), "Jeff's Thread")
    worker.start()
  }

  def firstRealExample(): Unit ={
    val w1 = new CalculatingWorker((x: Int) => { x * x + 2 })
    val w2 = new CalculatingWorker((x: Int) => { x + 1 })
    w1.start()
    w2.start()

    w1.join()     // Makes our "main" thread wait for the end of w1
    w2.join()

    CalculatingWorker.printX
  }

  def secondRealExample(): Unit ={
    val w1 = new CalculatingWorkerFix1((x: Int) => { x * x + 2 })
    val w2 = new CalculatingWorkerFix1((x: Int) => { x + 2 })
    w1.start()
    w2.start()

    w1.join()     // Makes our "main" thread wait for the end of w1
    w2.join()

    CalculatingWorkerFix1.printX  // Either 4 or 6 but never 2
  }

  def thirdRealExample(): Unit ={
    val w1 = new CalculatingWorkerFix1((x: Int) => { x * x + 2 })
    val w2 = new CalculatingWorkerFix1((x: Int) => { x + 2 })
    w1.start()
    w2.start()

    w1.join()     // Makes our "main" thread wait for the end of w1
    w2.join()

    CalculatingWorkerFix1.printX  // Either 4 or 6 but never 2
  }

  def bakeryExample(): Unit = {
    val bakerySize: Int = 5
    val breadsPerFactory: Int = 5
    val factoriesCount: Int = 7
    val clientsCount: Int = 4

    val factories = new Array[Factory](factoriesCount)
    val bakery = new Bakery(bakerySize, factories)

    for(i <- 0 until factories.length){
      factories(i) = new Factory(i, ((i * breadsPerFactory) to ((i + 1) * breadsPerFactory)).toList, bakery)
    }

    val clients = new Array[Client](clientsCount)
    for(i <- 0 until clients.length){
      clients(i) = new Client(i, bakery)
    }

    for(factory <- factories){
      factory.start()
    }
    for(client <- clients){
      client.start()
    }

    for(factory <- factories){
      factory.join()
    }
    for(client <- clients){
      client.join()
    }
  }

  def main(args: Array[String]): Unit = {
    // firstExample()
    //secondExample()
    // thirdExample()
    // firstRealExample()
    //secondRealExample()
    //thirdRealExample()
    bakeryExample()
  }
}