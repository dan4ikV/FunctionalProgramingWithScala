abstract class Sequence[+A] {
  def append(x: Sequence[A]): Sequence[A]
}