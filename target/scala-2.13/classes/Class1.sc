import java.util.Scanner

var s = new Scanner(System.in);

println("enter values of the first vector through enter: ")
val v1_1 = s.nextDouble();
val v1_2 = s.nextDouble();
println("enter values of the second vector through enter: ")
val v2_1 = s.nextDouble();
val v2_2 = s.nextDouble();

val vector1 = (v1_1, v1_2);
val vector2 = (v2_1, v2_2);

println("The difference between vectors is: " + difference2D(vector1, vector2));
println("Dot product of the vectors is: " + dotProduct(vector1, vector2));
println("Distance between the vectors is: " + distance(vector1, vector2));

println("Number of fire bits in 2: " + countFiredBits(2));
println("Number of fire bits in 2: " + countFiredBits(3));
println("Number of fire bits in 2: " + countFiredBits(4));
println("Number of fire bits in 2: " + countFiredBits(25));

def difference2D(vector1: (Double, Double), vector2: (Double, Double)): (Double, Double) = {
  (vector1._1 - vector2._1, vector1._2 - vector2._2)
}

def dotProduct(vector1: (Double, Double), vector2: (Double, Double)): Double = {
  vector1._1 * vector2._1 + vector1._2 * vector2._2
}

def distance(vector1: (Double, Double), vector2: (Double, Double)): Double = {
  var tempVector = difference2D(vector1, vector2)
  Math.sqrt(dotProduct(tempVector, tempVector))
}


def countFiredBits(number: Int): Int = {
  if (number == 0) {
    0;
  }
  else {
    countFiredBits(number >> 1) + (number & 1);
  }
}