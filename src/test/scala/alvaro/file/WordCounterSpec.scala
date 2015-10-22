package alvaro.file

import java.io._

import org.scalatest.{Matchers, WordSpecLike}
import scala.io.Source

object WordCounter {

  def count(f: File): Int = {
    f.isFile match {
      case true => Source.fromFile(f).getLines().map(_.split("\\s+").size).sum
      case false => f.listFiles().map(count(_)).sum
    }
  }
}

class WordCounterSpec extends WordSpecLike with Matchers {

  "file File1.txt" should {
    "has 5 words" in {
      WordCounter.count(
        new File(getClass.getClassLoader.getResource("File1.txt").getPath)
      ) shouldBe 5
    }
  }


  "file File2.txt" should {
    "has 1 word" in {
      WordCounter.count(
        new File(getClass.getClassLoader.getResource("File2.txt").getPath)
      ) shouldBe 1
    }
  }

  "file File3.txt" should {
    "has 0 word" in {
      WordCounter.count(
        new File(getClass.getClassLoader.getResource("File3.txt").getPath)
      ) shouldBe 0
    }
  }

  "directory dir1" should {
    "has counted 5 words below" in {
      WordCounter.count(
        new File(getClass.getClassLoader.getResource("dir1").getPath)
      ) shouldBe 5
    }
  }


  "directory dir2" should {
    "has counted 12 words below" in {
      WordCounter.count(
        new File(getClass.getClassLoader.getResource("dir2").getPath)
      ) shouldBe 12
    }
  }
}

