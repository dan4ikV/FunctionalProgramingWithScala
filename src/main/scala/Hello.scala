import java.util.Scanner

object Hello {
  def main(args: Array[String]): Unit = {
    binaryReverse(3);
    var s = new Scanner(System.in);

    println("enter values of the first vector through enter: ")
    val v1_1 = s.nextDouble();
    val v1_2 = s.nextDouble();
    println("enter values of the second vector through enter: ")
    val v2_1 = s.nextDouble();
    val v2_2 = s.nextDouble();

    val vector1 = (v1_1, v1_2);
    val vector2 = (v2_1, v2_2);

    sumList(List.range(1, 5))

    println("The difference between vectors is: " + difference2D(vector1, vector2));
    println("Dot product of the vectors is: " + dotProduct(vector1, vector2));
    println("Distance between the vectors is: " + distance(vector1, vector2));

    println("Number of fire bits in 2: " + countFiredBits(2));
    println("Number of fire bits in 2: " + countFiredBits(3));
    println("Number of fire bits in 2: " + countFiredBits(4));
    println("Number of fire bits in 2: " + countFiredBits(25));
  }

  def difference2D(vector1: (Double, Double), vector2: (Double, Double)): (Double, Double) = {
    (vector1._1 - vector2._1, vector1._2 - vector2._2)
  }

  def dotProduct(vector1: (Double, Double), vector2: (Double, Double)): Double = {
    vector1._1 * vector2._1 + vector1._2 * vector2._2
  }

  def distance(vector1: (Double, Double), vector2: (Double, Double)): Double =
    Math.sqrt(Math.pow(vector1._1 - vector2._1, 2) + Math.pow(vector1._2 - vector2._2, 2))


  def countFiredBits(number: Int): Int = {
    if (number == 0) {
      0;
    }
    else {
      countFiredBits(number >> 1) + (number & 1);
    }
  }

  def sumList(list: List[Int]): Int = {
    def go(list: List[Int], Accumulator: Int): Int = list match {
      case Nil => Accumulator
      case x :: tail => go(tail, Accumulator + x)
    }

    go(list, 0)
  }

  def binaryReverse(num: Int): Int = {
    if (num == 0) {
      0
    }
    else if(num == 1){
      1
    }
    else {
        if ((num & 1) == 1) {
          ((binaryReverse(num >> 1)) | Math.pow(2, num.toBinaryString.length - 1).toInt)
        }
        else {
          (binaryReverse(num >> 1))
        }
    }
  }
}
