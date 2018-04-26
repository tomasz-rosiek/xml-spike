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
        |  <!ELEMENT foo ANY>] >
        |  <foo>xxx</foo>
        |  """.stripMargin,
        true),
      (
        """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
         |            "http://www.w3.org/TR/html4/strict.dtd">
         |    <html>
         |    <head>
         |    </head>
         |    <body>
         |    </body>
         |    </html>""".stripMargin,
        false),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
          |  <!DOCTYPE foo [
          |  <!ELEMENT foo ANY >
          |  <!ENTITY xxe SYSTEM "file:///does/not/exist" >]>
          |  <foo>No entity in the body</foo>
          |  """.stripMargin,
        false),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
        |  <!DOCTYPE foo [
        |  <!ELEMENT foo ANY >
        |  <!ENTITY xxe SYSTEM "file:///does/not/exist" >]>
        |  <foo><<<<
        |  """.stripMargin,
        false),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
          |  <!DOCTYPE foo [
          |  <!ELEMENT foo ANY >
          |  <!ENTITY xxez PUBLIC "-//OASIS//DTD DocBook V4.1//EN" >]>
          |  <foo><<<<
          |  """.stripMargin,
        true),
      (
        """<?xml version="1.0" encoding="ISO-8859-1"?>
        |  <!DOCTYPE foo [
        |  <!ELEMENT foo ANY >
        |  <!ENTITY xxe PUBLIC "-//OASIS//DTD DocBook V4.1//EN" "file:///does/not/exist2" >]>
        |  <foo><<<<
        |  """.stripMargin,
        false)
    )

  "Should properly perform safety checks (manual analyzing)" in {
    forAll(samples) { (xml: String, expectedResult: Boolean) =>
      ExplicitAnalysisXMLSafetyChecker.checkIfSafe(new ByteArrayInputStream(xml.getBytes)) shouldBe expectedResult

    }
  }

  "Should properly perform safety checks (relying on XMLStreamParser)" in {
    forAll(samples) { (xml: String, expectedResult: Boolean) =>
      RelyingOnExceptionXMLSafetyChecker.checkIfSafe(new ByteArrayInputStream(xml.getBytes)) shouldBe expectedResult
    }
  }

}
