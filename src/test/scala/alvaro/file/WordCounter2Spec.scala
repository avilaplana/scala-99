package alvaro.file

import java.io.File

import org.scalatest.{Matchers, WordSpecLike}

import scala.io.Source

object WordCounter2 {
  def count(f: File): Map[String, Int] = {
    f.isFile match {
      case true =>
        val wordsInFile: Seq[String] = Source.fromFile(f).getLines().map(_.split("\\s+")).toSeq.flatten
        wordsInFile.groupBy(a => a).map(a => (a._1, a._2.size))
      case false =>
        val wordsInDir: Array[(String, Int)] = f.listFiles.map(count(_)).map(_.toSeq).flatten
        wordsInDir.groupBy(_._1).map(e => (e._1, e._2.map(_._2).sum))
    }
  }
}

class WordCounter2Spec extends WordSpecLike with Matchers {

  "file File1.txt" should {
    "has ocrruences word1 -> 2, word2 -> 1, word4 -> 1,  word5 -> 1" in {
      val wc = WordCounter2.count(
        new File(getClass.getClassLoader.getResource("File1.txt").getPath)
      )
      wc should contain("word1" -> 2)
      wc should contain("word2" -> 1)
      wc should contain("word4" -> 1)
      wc should contain("word5" -> 1)
    }
  }

  "file File2.txt" should {
    "has ocrruences word1 -> 1" in {
      WordCounter2.count(
        new File(getClass.getClassLoader.getResource("File2.txt").getPath)
      ) should contain("world1" -> 1)
    }
  }

  "file File3.txt" should {
    "has 0 word" in {
      WordCounter2.count(
        new File(getClass.getClassLoader.getResource("File3.txt").getPath)
      ) shouldBe Map.empty[String, Int]
    }
  }

  "directory dir1" should {
    "has ocrruences word1 -> 3,word2 -> 2" in {
      val wc = WordCounter2.count(
        new File(getClass.getClassLoader.getResource("dir1").getPath)
      )
      wc should contain("word1" -> 3)
      wc should contain("word2" -> 2)
    }
  }


  "directory dir2" should {
    "has counted 12 words below" in {
      val wc = WordCounter2.count(
        new File(getClass.getClassLoader.getResource("dir2").getPath)
      )
      wc should contain("world1" -> 1)
      wc should contain("world2" -> 3)
      wc should contain("world3" -> 3)
      wc should contain("world4" -> 2)
      wc should contain("world5" -> 2)
      wc should contain("world6" -> 1)

    }
  }
}
