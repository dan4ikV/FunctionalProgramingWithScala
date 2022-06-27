import java.util.concurrent.Semaphore

class Map(var map: collection.mutable.Map[Airport, (Int, Int)]) {
  def getDistance(airport1: Airport, airport2: Airport): Int = {
    val coords1 = map.get(airport1)
    val coords2 = map.get(airport2)
    math.sqrt(math.pow(coords1.get._1 - coords2.get._1, 2) + math.pow(coords1.get._2 - coords2.get._2, 2)).toInt
  }
}

class Airport(runways: Int, name: String) {
  val sem = new Semaphore(runways)

  def land(plane: Plane, nextAirport: String): Unit = {
    while (sem.availablePermits() <= 0) {
      println("Plane " + plane.getId() + " flying around airport " + name)
      plane.synchronized{
        plane.wait(80)
      }
    }
    sem.acquire()
    println("Plane " + plane.getId + " landed in airport " + name)
    plane.synchronized{
      plane.wait(scala.util.Random.nextInt(500) + 300)
    }
    if(nextAirport != "hangar") {
      println("Plane " + plane.getId + " fueled and flies to " + nextAirport)
    }
    sem.release()
  }

  def getName(): String = name
}

class Plane(path: List[Airport], map: Map) extends Thread {
  override def run(): Unit = {
    for (i <- 0 until path.size) {
      if (i + 1 < path.size) {
        path(i).land(this, path(i + 1).getName())
        val temp = map.getDistance(path(i), path(i + 1))
        println("Plane " + this.getId + " will be in flight for " + temp + " time units")
        this.synchronized {
          wait(temp)
        }
      }
      else {
        path(i).land(this, "hangar")
      }
    }
    println("Plane " + this.getId + " finishes the flight and goes to hangar.")
  }
}


object Concurrency {

  def main(args: Array[String]): Unit = {
    //creating airports
    print("Test 1")
    val airportWroclaw = new Airport(3, "Wroclaw")
    val airportKyiv = new Airport(3, "Kyiv")
    val airportKyoto = new Airport(4, "Kyoto")
    val airportNewYork = new Airport(8, "New York")
    val airportKobendhavn = new Airport(3, "Kobendhavn")
    val airportFrankivsk = new Airport(2, "Ivano-Frankivsk")

    //generating a map with different cities
    var map = collection.mutable.Map[Airport, (Int, Int)]()
    map += (airportWroclaw -> (scala.util.Random.nextInt(1000), scala.util.Random.nextInt(1000)))
    map += (airportKyiv -> (scala.util.Random.nextInt(1000), scala.util.Random.nextInt(1000)))
    map += (airportKyoto -> (scala.util.Random.nextInt(1000), scala.util.Random.nextInt(1000)))
    map += (airportNewYork -> (scala.util.Random.nextInt(1000), scala.util.Random.nextInt(1000)))
    map += (airportKobendhavn -> (scala.util.Random.nextInt(1000), scala.util.Random.nextInt(1000)))
    map += (airportFrankivsk -> (scala.util.Random.nextInt(1000), scala.util.Random.nextInt(1000)))

    val mapForPlanes = new Map(map)

    //getting list of the airports from the map
    val airportList = map.toList.unzip._1

    var planes = List[Plane]()
    for (i <- 0 to 20) {
      planes = new Plane(generatePath(airportList, scala.util.Random.nextInt(10) + 10), mapForPlanes) :: planes
    }

    for (i <- 0 to 20) {
      planes(i).start()
    }
    for (i <- 0 to 20) {
      planes(i).join()
    }



    print("\n\n\n\nTest 2\n")

    //test for flying around and having the same route for all the planes
    //also each of the plane flies for the same time, which means time is calculated correctly
    val plane1 = new Plane(List(airportKyiv, airportFrankivsk), mapForPlanes)
    val plane2 = new Plane(List(airportKyiv, airportFrankivsk), mapForPlanes)
    val plane3 = new Plane(List(airportKyiv, airportFrankivsk), mapForPlanes)
    val plane4 = new Plane(List(airportKyiv, airportFrankivsk), mapForPlanes)
    val plane5 = new Plane(List(airportKyiv, airportFrankivsk), mapForPlanes)
    val plane6 = new Plane(List(airportKyiv, airportFrankivsk), mapForPlanes)

    plane1.start()
    plane2.start()
    plane3.start()
    plane4.start()
    plane5.start()
    plane6.start()

    plane1.join()
    plane2.join()
    plane3.join()
    plane4.join()
    plane5.join()
    plane6.join()

  }

  def generatePath(allAirports: List[Airport], num: Int): List[Airport] = {
    var temp = List[Airport]()
    var prev = -1
    for (i <- 0 to num) {
      var curr = scala.util.Random.nextInt(allAirports.size)
      while (curr == prev){
        curr = scala.util.Random.nextInt(allAirports.size)
      }
      temp = allAirports(curr) :: temp
      prev = curr
    }
    temp
  }
}
