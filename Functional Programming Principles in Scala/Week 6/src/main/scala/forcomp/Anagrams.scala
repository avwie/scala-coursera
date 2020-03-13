package forcomp

object Anagrams extends AnagramsInterface {

  type Word = String
  type Sentence = List[Word]
  type Occurrence = (Char, Int)
  type Occurrences = List[Occurrence]

  val dictionary: List[Word] = Dictionary.loadDictionary

  def wordOccurrences(w: Word): Occurrences = {
    for {
      (k, v) <- w.toLowerCase.toSeq.groupBy(c => c)
    } yield (k -> v.length)
  }.toList.sortBy { case (c, i) => c }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences(s.mkString(""))

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(word => wordOccurrences(word)).withDefaultValue(List())

  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  def combinations(occurrences: Occurrences): List[Occurrences] =
    occurrences match {
      case Nil => List(Nil)
      case (c, i) :: tail => {
        val tailCombinations = combinations(tail)
        (for {
          occs <- tailCombinations
          j <- 1 to i
        } yield (c, j) :: occs) ++ tailCombinations
      }
    }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def subtractSingle(x: Occurrences, occ: Occurrence): Occurrences =
      (x, occ) match {
        case (List(), _)                          => x
        case (head :: tail, occ) if (head == occ) => tail
        case ((char, count) :: tail, (occ_char, occ_count))
            if (char == occ_char && count != occ_count) =>
          (char, count - occ_count) :: tail
        case (head :: tail, occ) => head :: subtractSingle(tail, occ)
      }
    y match {
      case List()       => x
      case head :: tail => subtract(subtractSingle(x, head), tail)
    }
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def inner(occurrences: Occurrences): List[Sentence] = {
      if (occurrences.isEmpty) List(Nil)
      else
        for {
          occurrence <- combinations(occurrences)
          word <- dictionaryByOccurrences(occurrence)
          sentence <- inner(subtract(occurrences, occurrence))
        } yield word :: sentence
    }
    if (sentence.isEmpty) List(Nil)
    else {
      inner(sentenceOccurrences(sentence))
    }
  }
}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(
        List("forcomp", "linuxwords.txt").mkString("/", "/", "")
      )
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
