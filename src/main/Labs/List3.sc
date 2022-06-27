//task 1
sealed trait Tree[A]
      case class Leave[A](value: A) extends Tree[A]
      case class OneChild[A, B](child : Tree[A], func : Tree[A] => B) extends Tree[B]
      case class TwoChildren[A, B, C](child1: Tree[A], child2 : Tree[B], func : (Tree[A], Tree[B]) => C) extends Tree[C]

def evaluate[A](tree: Tree[A]): A = tree match {
  case Leave(x) => x
  case OneChild(x, func) => func(x)
  case TwoChildren(x, y, func) => func(x, y)
}

//test from the task
val leave1 = Leave(1.0)
val leave2 = Leave(5)

val singleChildBranch1 = OneChild(leave1, (x: Tree[Double]) => evaluate(x) * -1)
val singleChildBranch2 = OneChild(leave2, (x: Tree[Int]) => evaluate(x).toDouble)

val twoChildrenBranch = TwoChildren(singleChildBranch1, singleChildBranch2, (x: Tree[Double], y: Tree[Double]) => evaluate(x) + evaluate(y))
evaluate(twoChildrenBranch)

//test2
val leave3 = Leave("Foo")
val twoChildrenBranch2 = TwoChildren(twoChildrenBranch, leave3, (x: Tree[Double], y : Tree[String]) => List.fill(evaluate(x).toInt)(evaluate(y)))
evaluate(twoChildrenBranch2)

//test3 LazyTree
val lazyTree : (Tree[BigInt], Tree[BigInt], BigInt) => TwoChildren[BigInt, BigInt, BigInt] = (num: Tree[BigInt], pow: Tree[BigInt], to: BigInt) => if(evaluate(pow) < to){lazyTree(TwoChildren(num, pow, (a: Tree[BigInt], b: Tree[BigInt]) => evaluate(a) * evaluate(b)), Leave(evaluate(pow) + 1), to)} else {TwoChildren(num, pow, (a: Tree[BigInt], b: Tree[BigInt]) => evaluate(a) * evaluate(b))}
evaluate(lazyTree(Leave(2), Leave(3), 8))

//task2
sealed trait HtmlTag{
  val open : String
  val close : String
}

case class Document(head: Head, body: Body) extends HtmlTag {
  val open = "<!DOCTYPE html>\n<html>\n"
  val close = "</html>"
  override def toString(): String = {
    open + head.toString + body.toString + close;
  }
}

case class Head(title : Option[Title]) extends HtmlTag {
  val open = "<head>\n"
  val close = "</head>\n"
  override def toString(): String = title match {
    case None => open + close
    case Some(a) => open + a.toString + close
  }
}

case class Title(val text : String) extends HtmlTag{
  val open = "<title>"
  val close = "</title>\n"
  override def toString(): String = {
    open + text + close
  }
}

case class Body(text : Option[List[Text]]) extends HtmlTag {
  val open = "<body>\n"
  val close = "</body>\n"
  override def toString(): String = {
    def addText(text : Option[List[Text]]): String = text match {
      case None => ""
      case Some(Nil) => ""
      case Some(a) => a.head.toString + addText(Some(a.tail))
    }
    open + addText(text) + close
  }
}

sealed trait Text extends HtmlTag
  case class Header(size: Int, text: String) extends Text{
    val open = "<h" + size + ">"
    val close = "</h" + size + ">\n"
    override def toString(): String = {
      open + text + close
    }
  }
  case class Paragraph(text: String) extends Text{
    val open = "<p>"
    val close = "</p>\n"
    override def toString(): String = {
      open + text + close
    }
  }
case class Picture(address: String) extends Text{
  val open = "<img src=\""
  val close = "\">\n"
  override def toString(): String = {
    open + address + close
  }
}

val text = List(Header(1, "First header"), Paragraph("text"), Header(3, "Second header"),  Paragraph("text2"))
val body = Body(Some(text))
val title = Title("Hello")
val head = Head(Some(title))
val document = Document(head, body)

def parse(document: Document): String = {
  document.toString()
}

def quickCreate(title: Option[String], text: Option[List[Text]]): Document = {
    val body = Body(text)
    val head = title match{
      case None => Head(None)
      case Some(a) => Head(Some(Title(a)))
    }
    Document(head, body)
}

parse(document)
parse(quickCreate(None, None))
parse(quickCreate(None, Some(List(Header(1, "hi"), Paragraph("My name is danylo")))))
parse(quickCreate(None, Some(List(Header(1, "Dolonky"), Paragraph("Kids studio dolonky"), Picture("https://scontent.fifo3-1.fna.fbcdn.net/v/t1.0-9/16712030_1693604037597609_9223163300456946556_n.jpg?_nc_cat=111&ccb=2&_nc_sid=825194&_nc_ohc=7IyMPWOpwlUAX_CC7oS&_nc_ht=scontent.fifo3-1.fna&oh=57623b8fb53372384bebd0f05496e3fc&oe=5FDA7DD9")))))


