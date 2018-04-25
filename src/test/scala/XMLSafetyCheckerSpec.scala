import java.io.ByteArrayInputStream

import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.TableDrivenPropertyChecks._

class XMLSafetyCheckerSpec extends WordSpec with Matchers {

  val samples =
    Table(
      ("XML", "expected result"),
      ("<html></html>", true),
      ("<html><<", true),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
        |  <!DOCTYPE foo [
        |  <!ELEMENT foo ANY >
        |  <foo>&xxe;</foo>
        |  """.stripMargin,
        true),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
        |  <!DOCTYPE foo [
        |  <!ELEMENT foo ANY >
        |  <!ENTITY xxe SYSTEM "file:///does/not/exist" >]>
        |  <foo>&xxe;</foo>
        |  """.stripMargin,
        false),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
        |  <!DOCTYPE foo [
        |  <!ELEMENT foo ANY >
        |  <!ENTITY xxe SYSTEM "file:///does/not/exist" >]>
        |  <foo><<<<
        |  """.stripMargin,
        false)
    )

  "Should properly perform safety checks" in {
    forAll(samples) { (xml: String, expectedResult: Boolean) =>
      XMLSafetyChecker.checkIfSafe(new ByteArrayInputStream(xml.getBytes)) shouldBe expectedResult

    }
  }

}
