type SectionLength = Int
type ChapterSectionsLengths = List[SectionLength]
type BookDescription = List[ChapterSectionsLengths]

case class Section(startingPage: Int, endingPage: Int)
case class Chapter(sections: List[Section])
case class Book(chapters: List[Chapter])

def makeASection(startingPage : Int, length : SectionLength): Section = {
  Section(startingPage, startingPage + length - 1)
}

def makeAChapter(startingPage: Int, sectionsList: ChapterSectionsLengths): Chapter ={
  def makeSections(startingPage: Int, list: ChapterSectionsLengths):List[Section] = list match {
    case Nil => List()
    case h :: tail => makeASection(startingPage, h) :: makeSections(startingPage + h, tail)
  }
  Chapter(makeSections(startingPage, sectionsList))
}

def makeChapters(startingPage: Int, list : BookDescription): List[Chapter] = {
  list match {
    case Nil => List()
    case h :: tail =>
      val chapter = makeAChapter(startingPage, h)
      chapter :: makeChapters(chapter.sections(chapter.sections.size - 1).endingPage + 1, tail)
  }
}

def makeBook(list: BookDescription): Book ={
  Book(makeChapters(1, list))
}

val chapter1 = List(1, 2, 5)
val chapter2 = List(7, 7, 9)
val chapter3 = List(2, 4, 4)
makeBook(List(chapter1, chapter2, chapter3))

val chapter4 = List(10, 12, 12, 12, 1)
makeBook(List(chapter1, chapter2, chapter3, chapter4))