package net.nomadicalien.ch10

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification


case class Word(w: String) extends AnyVal

object ArbitraryPhrase {
    /**
      * Source https://en.wikipedia.org/wiki/Most_common_words_in_English
      * */
    implicit def arbWord: Arbitrary[Word] =
        Arbitrary(
            Gen.oneOf(
                "the",
                "be",
                "to",
                "of",
                "and",
                "a",
                "in",
                "that",
                "have",
                "I",
                "it",
                "for",
                "not",
                "on",
                "with",
                "he",
                "as",
                "you",
                "do",
                "at",
                "this",
                "but",
                "his",
                "by",
                "from",
                "they",
                "we",
                "say",
                "her",
                "she",
                "or",
                "an",
                "will",
                "my",
                "one",
                "all",
                "would",
                "there",
                "their",
                "what",
                "so",
                "up",
                "out",
                "if",
                "about",
                "who",
                "get",
                "which",
                "go",
                "me",
                "when",
                "make",
                "can",
                "like",
                "time",
                "no",
                "just",
                "him",
                "know",
                "take",
                "people",
                "into",
                "year",
                "your",
                "good",
                "some",
                "could",
                "them",
                "see",
                "other",
                "than",
                "then",
                "now",
                "look",
                "only",
                "come",
                "its",
                "over",
                "think",
                "also",
                "back",
                "after",
                "use",
                "two",
                "how",
                "our",
                "work",
                "first",
                "well",
                "way",
                "even",
                "new",
                "want",
                "because",
                "any",
                "these",
                "give",
                "day",
                "most",
                "us",
            ).map(Word.apply)
        )
}

class WordCountSpec extends Specification with ScalaCheck {

    import net.nomadicalien.ch10.Monoids.WordCount._
    import ArbitraryPhrase._

    "count with monoid must match naiive impl" >> prop { p: List[Word] =>
        val phrase = p.map(_.w).mkString(" ")

        countWords(phrase) must_== countViaFold(phrase)


    }
}
