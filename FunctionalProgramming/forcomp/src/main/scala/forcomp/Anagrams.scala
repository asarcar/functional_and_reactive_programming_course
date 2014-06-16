package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  type Elem = (Char, Int)

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  type SetOccurrences = Set[Occurrences]

  type MapOccurrences = Map[Char, Int]

  type WordOccurrences = List[Word]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences =
    w.toList.groupBy(char=>char.toLower).mapValues(l=>l.length).
      toList.sortWith((x, y) => (x._1 < y._1))

  /*
   * Debug Routine:
   * Converts a sentence in string format to List of Words: sentence format
   */
  def strToSentence(sentence: String): Sentence = {
    def isAlphabet(ch: Char): Boolean =
      (((ch >= 'a') && (ch <= 'z')) || ((ch >= 'A') && (ch <= 'Z')))

    def strToWords(remsen: String, curWord: Word, words: Sentence): Sentence = {
      if (remsen.isEmpty) {
        if (curWord.isEmpty)
          words
        else
          curWord :: words
      } else if (isAlphabet(remsen.head)) {
        strToWords(remsen.tail, curWord + remsen.head.toLower, words)
      } else {
        if (curWord.isEmpty)
          strToWords(remsen.tail, "", words)
        else
          strToWords(remsen.tail, "", curWord :: words)
      }
    }

    strToWords(sentence, "", List())
  }

  def listSenToStr(ls: ListSentence) = ls./:("")((str, s) => str + "\n" + s.mkString(" "))

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = 
    wordOccurrences(s./:("")((x, y) => x+y))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(word=>wordOccurrences(word))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = 
    dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if (occurrences.isEmpty) {
      List(List())
    } else {
      val remOccList = combinations(occurrences.tail)
      /*
       * head (occurences.head) character can occur anywhere from 0 to maximum number
       * that is in its frequency
       */
      for (remList <- remOccList; freq <- List.range(0,occurrences.head._2+1))
      yield (if (freq == 0) remList else (occurrences.head._1, freq) :: remList)
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def subtractElem(xl: MapOccurrences, ye: Elem): MapOccurrences = {
      require(!xl.isEmpty, "subtract on Nil List")
      val remainingFreq = xl.apply(ye._1) - ye._2
      if (remainingFreq == 0)
        xl - ye._1
      else
        xl.updated(ye._1, remainingFreq)
    }
    def subtractList(xl: MapOccurrences, yl: Occurrences): MapOccurrences = {
      if (yl.isEmpty)
        xl
      else
        subtractList(subtractElem(xl, yl.head), yl.tail)
    }
    subtractList(x.toMap, y).toList.sortWith((v1, v2) => (v1._1 < v2._1))
  }

  /**
    * Converts an occurrence to a unique String
    */
  def occurToStr(occ: Occurrences): String =
    occ.flatMap(x => ((1 to x._2) map (i => x._1))).mkString

  def compare(occ1: Occurrences, occ2: Occurrences): Int =
    occurToStr(occ1) compare occurToStr(occ2)

  /*
   * type Word = String
   * type Sentence = List[Word]
   * type Elem = (Char, Int)
   * type Occurrences = Sorted List[Elem]
   */
  type MapOccur = Map[Occurrences, Int]

  def mapOccurToStr(mapOcc: MapOccur) =
    mapOcc.flatMap(x => ((1 to x._2) flatMap
      (i => occurToStr(x._1)))).toList.sorted.mkString

  def mapOccurToOccur(mapOcc: MapOccur): Occurrences =
    wordOccurrences(mapOccurToStr(mapOcc))

  type ListMapOccur = List[MapOccur]
  type ListSentence = List[Sentence]

  /*
   * Given an occurance list: this generates the list of MapOccur
   * where each list entry is a mapping of the occurence of a valid word
   * and the number of times the occurence is replicated
   */
  def genListMapOccur(occurs: Occurrences): ListMapOccur = {
    /*
     * Given occ we generate a list of mapped occurances with
     * the additional constraint that the string representation
     * of every occurance generated in the List is not less than the
     * maxOccSoFar
     */
    def genListMapLargerOccur(occ: Occurrences, maxOccSoFar: Occurrences):
        ListMapOccur = {
      if (occ.isEmpty) {
        List(Map() withDefaultValue 0)
      } else {
        val combOccurs = combinations(occ)
        // println("combinations = " + combOccurs)
        for {
          occur <- combOccurs
          if (dictionaryByOccurrences.contains(occur) &&
              (compare(occur, maxOccSoFar) >= 0))
          remOccur = subtract(occ, occur)
          remListMap = genListMapLargerOccur(remOccur, occur)
          mapOccur <- remListMap
        } yield add(mapOccur, occur)
      }
    }
    genListMapLargerOccur(occurs, Nil)
  }

  def combineSetOfAllOccurs(listMapOccurs: ListMapOccur): SetOccurrences = 
    (listMapOccurs.flatMap(mapOccur => mapOccur.map{case (occ, freq) => occ})).toSet

  def mapOccurToSet(mapOccur: MapOccur): SetOccurrences =
    mapOccur.map{case (occ, freq) => occ}.toSet

  /**
    * Adds occurance y to xMap: the mapping table
    * that keeps occurance and frequence of occurance
    */
  def add(xMap: MapOccur, y: Occurrences): MapOccur =
    xMap.updated(y, xMap(y) + 1)

  /**
    * Subtracts occurance y from xMap: the mapping table
    * that keeps occurance and frequence of occurance
    */
  def subtract(xMap: MapOccur, y: Occurrences): MapOccur = {
    val freq = xMap(y)
    assert((freq >= 1), "Map=(" + y + "->" + freq + ") subtract!")
    if (freq == 1) {
      xMap - y
    } else {
      xMap.updated(y, freq - 1)
    }
  }

  type MapOccStrListSen = Map[String, ListSentence]

  /*
   * Given a mapping of occurances to frequency of occurances
   * generate all the permutation of sentences that can be created
   *
   * Note: genListSen remembers the List of Sentences created for a
   * particular occurrence list so that it can be referenced later.
   * e.g. Given "the wild cow ate" will not now go through
   * the entire recursion and process of generating all
   * permutations of "ox ate" ("cow ate", "cow tea", "cow eat",
   * "ate cow", "tea cow", "eat cow", ...) given the prefix of
   * all anagrams of "the wild" ("the wild", "wild the", ...)
   */
  def genListSen(mapOccur: MapOccur): ListSentence = {
    if (mapOccur.isEmpty) {
      List(List())
    } else {
      for {
        (occ, freq) <- mapOccur
        word <- dictionaryByOccurrences(occ)
        remMapOcc = subtract(mapOccur,occ)
        remListSen = genListSen(remMapOcc)
        remSen <- remListSen
      } yield {
        // println("word=" + word + ": remSen=" + remSen)
        (word :: remSen)
      }
      /*
       * The first for generator is of type MapOccur whereas we
       * want to return ListSentence. Hence the type checking would
       * fail unless we do not convert the return type to List
       */
    }.toList
  }

  /*
   * AnagramGenCache caches all the permutations of sentences possible
   * as anagrams of a sentence.
   * It starts with a base case of an List with only the empty sentence
   * and then builds up the list of sentences for all other cases.
   */
  class AnagramGenCache(occurSet: SetOccurrences, listSen: ListSentence) {
    private lazy val finalList = listSen
    private lazy val mapOccToAnagramGenCache = {
      for {
        occ <- occurSet
        lSen = {
          for {
            word <- dictionaryByOccurrences(occ)
            sen <- listSen
          } yield word :: sen
        }
      } yield {
        (occ, new AnagramGenCache(occurSet, lSen))
      }
    }.toMap
    private def firstWordPermutations(mapOcc: MapOccur): ListSentence = {
      if (mapOcc.isEmpty) {
        finalList
      } else {
        for {
          (occ, freq) <- mapOcc
          remMapOcc = subtract(mapOcc,occ)
          /*
           * Used the cached values to generate all the possible list of sentences
           * from remMapOcc
           */
          listSen = mapOccToAnagramGenCache(occ).firstWordPermutations(remMapOcc)
          sen <- listSen
        } yield sen
      }.toList
    }

    def apply(mapOccur: MapOccur): ListSentence = firstWordPermutations(mapOccur)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): ListSentence = {
    val senOccur: Occurrences = sentenceOccurrences(sentence)
    // println("senOccur = " + senOccur)
    val listMap: ListMapOccur = genListMapOccur(senOccur)
    // println("listMap = " + listMap)
    /*
     * Create one superset of all mapOccurences that can be gleaned
     * from this sentence. This will be used as the superset for
     * the caching algorithm.
     * One superset caused us to explode too much of the solution space
     * Now we are exploring only Occurrences relevant for the permutation
     * val combSetOccur: SetOccurrences = combineSetOfAllOccurs(listMap)
     *  val anagramGenFn = new AnagramGenCache(combSetOccur, List(List()))
     */

    for {
      mapOccur <- listMap
      combSetOccur = mapOccurToSet(mapOccur)
      anagramGenFn = new AnagramGenCache(combSetOccur, List(List()))
      sentence <- anagramGenFn(mapOccur)
      // sentence <- genListSen(mapOccur)
    } yield {
      // println("mapOccur=" + mapOccur + ": sentence=" + sentence)
      sentence
    }
  }

  class MemoStringFunction[T](f: String => T, base: String) {
    private lazy val v = f(base)
    private lazy val ts = (for {
      tx <- 'a' to 'z'
    } yield (tx, new MemoStringFunction(f, base + tx))).toMap
    private def get(str: String): T = str match {
      case "" => v
      case non_empty_str => ts(non_empty_str.head).get(non_empty_str.tail)
    }
    def apply(s: String): T = get(s)
  }

  def main(args: Array[String]) {
    val t0 = System.nanoTime()
    val str1 = ""
    val ls1 = sentenceAnagrams(strToSentence(str1))
    println("Anagrams of " + str1 + ": #(" +  ls1.length + "): " +  
      listSenToStr(ls1))
    println(List.range(1, 20).map(i => '-').mkString)
    println("# of anagrams: " + ls1.length)
    println(List.range(1, 20).map(i => '*').mkString)
    val str2 = "tea"
    val ls2 = sentenceAnagrams(strToSentence(str2))
    println("Anagrams of " + str2 + ": #(" +  ls2.length + "): " +  
      listSenToStr(ls2))
    println(List.range(1, 20).map(i => '-').mkString)
    println("# of anagrams: " + ls2.length)
    println(List.range(1, 20).map(x => '*').mkString)
    val str3 = "I love you"
    val ls3 = sentenceAnagrams(strToSentence(str3))
    println("Anagrams of " + str3 + ": #(" +  ls3.length + "): " +  
      listSenToStr(ls3))
    println(List.range(1, 20).map(i => '-').mkString)
    println("# of anagrams: " + ls3.length)
    println(List.range(1, 20).map(x => '*').mkString)
    val str4 = "Mickey Mouse"
    val ls4 = sentenceAnagrams(strToSentence(str4))
    println("Anagrams of " + str4 + ": #(" +  ls4.length + "): " +  
      listSenToStr(ls4))
    println(List.range(1, 20).map(i => '-').mkString)
    println("# of anagrams: " + ls4.length)
    println(List.range(1, 20).map(x => '*').mkString)
    val str5 = str2 + " " + str3 // + " " + str4
    val ls5 = sentenceAnagrams(strToSentence(str5))
    println("Anagrams of " + str5 + ": #(" +  ls5.length + "): " +  
      listSenToStr(ls5))
    println(List.range(1, 20).map(i => '-').mkString)
    println("# of anagrams: " + ls5.length)
    println(List.range(1, 20).map(x => '*').mkString)
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
  }
}
  
