package net.nomadicalien.ch10

object Monoids {
  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }

  /**
    *  EXERCISE 10.7
    */
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty) {
      m.zero
    } else if(v.size == 1) {
      f(v(0))
    } else {
      val half = v.size / 2
      val (firstHalf, secondHalf) = v.splitAt(half)
      m.op(foldMapV(firstHalf, m)(f), foldMapV(secondHalf, m)(f))
    }
  }

  // 10.8 10.9 Hard - skipping

 // Write a monoid instance for WC and make sure that it meets the monoid laws. val wcMonoid: Monoid[WC]

  sealed trait WordCount
  case class Stub(chars: String) extends WordCount
  case class Part(lStub: String, countOfWords: Int, rStub: String) extends WordCount

  // "lorem ipsum dolor sit amet, "
  // "lorem ipsum do" "lor sit amet, "
  // "lorem i" "psum do" "lor sit" " amet, "
  // "lor" "em i" "psu" " do" "lor" " sit" " am" "et, "



  // "lor|em ips|um d|o"
  // Part(lorem 0 ips) , Part(um 0 do)

  import WordCount._
  val wcMonoid = new Monoid[WordCount] {
    def op(a1: WordCount, a2: WordCount): WordCount = (a1, a2) match {
      case (Stub(a), Stub(b)) =>  Stub(a + b)
      case (Part(l1, leftWordCount, r1), Part(l2, rightWordCount, r2)) =>
        val middleCount = countWords(r1 + l2)
        Part(l1, leftWordCount + rightWordCount + middleCount , r2)

      case (Stub(a), Part(l, wordCount, r)) =>
        val left = a + l
        val countLeft = countWords(left)
        Part(left, wordCount + countLeft, r)

      case (Part(l, wordCount, r), Stub(a)) =>
        val right = r + a
        val countRight = countWords(right)
        Part(l, wordCount + countRight, r + a)

    }

    def zero: WordCount = Stub("")
  }

  object WordCount {

    def countWords(phrase: String) = {
      val words = phrase.replaceAll("""[^\w ]"""", "").trim
       if(words.isEmpty) {
         0
       } else {
         words.split(' ').length
       }

    }


    def apply(chars: String): WordCount = {
      if (chars.trim.isEmpty) Part("", 0, "")
      else Stub(chars)
    }


    def countViaFold(phrase: String): Int = {
      val result = foldMapV(phrase.toIndexedSeq, wcMonoid)(c => WordCount(c.toString))
      result match {
        case Stub(c) => if (c.trim.isEmpty) 0 else 1
        case Part(_, countOfWords, _) => countOfWords
      }
    }

  }


}