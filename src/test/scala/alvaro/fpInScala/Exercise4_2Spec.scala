package alvaro.fpInScala

import alvaro.fpInScala.exercise4_1
import alvaro.fpInScala.exercise4_1.{None, Option}
import org.scalatest.{Matchers, WordSpecLike}

//EXERCISE 2:
//Implement the variance function (if the mean is m, variance is the mean of math.pow(x - m, 2), see definition) in terms of mean and
//flatMap.
//Variance can actually be computed in one pass, but for pedagogical purposes we will compute it using two passes.
//The first will compute the mean of the data set, and the second will compute the mean squared difference from this mean.
class Exercise4_2Spec extends WordSpecLike with Matchers {

  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] = {
      xs match {
        case Nil => None
        case seq => exercise4_1.Some(seq.sum / seq.size)
      }
    }

    mean(xs).flatMap(m => mean(xs.map(e => math.pow(e - m, 2))))
  }


  "variance" should {
    "return 0.66666666666666666 for the list 1,2,3" in {
      variance(Seq(1, 2, 3)) shouldBe exercise4_1.Some(0.66666666666666666)
    }

    "return 2.91666666666666666 for the list 1,2,3,4,5,6" in {
      variance(Seq(1, 2, 3, 4, 5, 6)) shouldBe exercise4_1.Some(2.91666666666666666)
    }
  }
}
