package forcomp

import scala.io.{ Codec, Source }

object Anagrams extends AnagramsInterface:

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

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

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = 
    (for (elem, e_list) <- w.toList.groupBy((character: Char) => character.toLower)
    yield (elem, e_list.length)
    ).toList.sortWith(_._1 < _._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = 
    wordOccurrences(s.mkString)

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
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy((word: String) => wordOccurrences(word))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)

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
  def combinations(occurrences: Occurrences) : List[Occurrences] = {
    val allSubsets = for {
      // For each character and its count, generate all counts from 0 to the count
      combination <- occurrences.map { case (char, count) =>
        (0 to count).map(c => (char, c))
      }.foldLeft(List(List.empty[(Char, Int)])) { (acc, counts) =>
        for {
          subset <- acc
          count <- counts
        } yield subset :+ count
      }
    } yield combination

    // Remove entries where count is 0 to produce valid subsets
    allSubsets.map(_.filter { case (_, count) => count > 0 })
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
  def subtract(x: Occurrences, y: Occurrences): Occurrences = 
    val map2 = y.toMap
    x.foldLeft((List.empty[(Char, Int)])) {
      case (result, (char, freq)) => 
        val query = map2.getOrElse(char, 0)
        if (freq - query <= 0) then result else result ::: List((char, freq - query))
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
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =
    val computed: Map[List[Occurrences], Set[Occurrences]] = Map()
    def occurrencesExists(occurrence: Occurrences): Boolean = dictionaryByOccurrences.getOrElse(occurrence, Nil) != Nil
    def occurrenceSize(occurrence: Occurrences): Int = occurrence.foldLeft(0)((count, occ) => count + occ._2)

    def computeAnagrams(
      current: List[Occurrences], 
      remaining: List[Occurrences], 
      sub_acc: Set[Occurrences], 
      main_acc: Set[Set[Occurrences]], 
      count: Int
      ): List[Set[Occurrences]] =
      
      if (remaining.isEmpty) || count <= 0 then main_acc.toList
      else current match
        // for each entry in the query list
        case head :: next =>
          if occurrenceSize(head) == count then 
            val anagram = sub_acc + head
            computeAnagrams(next, remaining, sub_acc, main_acc + anagram, count)
          else
            val new_remaining = remaining.map(occ => subtract(occ, head)).filter(occ => !occ.isEmpty).distinct
            val filter_remaining = (for occur <- new_remaining if occurrencesExists(occur) yield occur)

            if filter_remaining.isEmpty then computeAnagrams(next, remaining, sub_acc, main_acc, count)
            else 
              val new_size = count - occurrenceSize(head)
              val sub_anagrams = computeAnagrams(filter_remaining, new_remaining, sub_acc + head, Set(), new_size)
              computeAnagrams(next, remaining, sub_acc, main_acc ++ sub_anagrams, count)
        case Nil => main_acc.toList

    if sentence.isEmpty then List(Nil)
    else
      val occs = sentenceOccurrences(sentence)
      val combs = combinations(occs)
      val combs2 = for comb <- combs if occurrencesExists(comb) yield comb
      val sets = computeAnagrams(combs2, combs, Set(), Set(), occurrenceSize(occs))
      val perm_list = for {
        ls <- sets.map((st) => st.toList)
        perm <- ls.permutations.toList.map((occs) => { 
          for { 
            occ <- occs 
          } yield dictionaryByOccurrences.getOrElse(occ, Nil) 
        })
      } yield perm.foldLeft(List(List.empty[String])) { (acc, w_list) => 
          for {
            lst <- acc
            word <- w_list
          } yield word :: lst 
      }
      perm_list.foldLeft(List(List.empty[String])) { (acc, curr) => 
        (for lst <- curr yield lst) ::: acc}.filter((ls) => !ls.isEmpty)


    
    
    

    

object Dictionary:
  def loadDictionary: List[String] =
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try
      val s = Source.fromInputStream(wordstream)(Codec.UTF8)
      s.getLines().toList
    catch
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    finally
      wordstream.close()
