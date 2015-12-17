package alvaro.json

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{Writes, Json}

case class Domain1(name: String, description: String)

object Domain1 {
  implicit val fmt = Json.format[Domain1]
}

object JsonParser {
  def toJson[T <: AnyRef](d: T)(implicit wrt: Writes[T]): String = Json.toJson(d).toString()
}

class GenercicJsonSpec extends WordSpec with Matchers {
  
  "toJson" should {
    "be transform to json the generic T as Domain1" in {
      JsonParser.toJson(Domain1("name1", "description1")) shouldBe """{"name":"name1","description":"description1"}"""
    }
  }
}
