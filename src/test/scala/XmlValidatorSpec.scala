/**
  * Created by joanna.pinto on 24/04/2018.
  */

import org.scalatest._

class XmlValidatorSpec extends WordSpec with Matchers {


  val unsafeXml = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" +
    "  <!DOCTYPE foo [ " +
    "  <!ELEMENT foo ANY >" +
    "  <!ENTITY xxe SYSTEM \"file:///does/not/exist\" >]>" +
    "<foo>&xxe;</foo>"


  private val safeXml = "<foo>hello<bar>world</bar></foo>"

  private val partialXml = "<foo>hello<bar>world</bar>"

  "XmlValidator safe parse" should {

    "return success (Right) if the bytes are XML that is valid and safe" in {
      val xmlValidator = new XmlValidator()
      val result = xmlValidator.secureValidate(safeXml.getBytes)

      result.isRight shouldBe true
    }

    "return the expected Left wrapped error message if bytes cannot be parsed as correctly formatted XML" in {
      val xmlValidator = new XmlValidator()
      val result = xmlValidator.secureValidate("Hello World".getBytes)

      result.isLeft shouldBe true

      val errorMsg: String = result.left.get
      errorMsg.contains("ParseError") shouldBe true
      errorMsg.contains("Content is not allowed in prolog") shouldBe true
    }

    "return the expected Left wrapped error message if bytes are unsafe XML" in {
      val xmlValidator = new XmlValidator()
      val result = xmlValidator.secureValidate(unsafeXml.getBytes)

      result.isLeft shouldBe true

      val errorMsg: String = result.left.get
      errorMsg.contains("ParseError") shouldBe true
      errorMsg.contains("The entity \"xxe\" was referenced, but not declared.") shouldBe true
    }

    "return success (Right) if the bytes are XML that is valid and safe AND CAN BE STREAMED" in {
      val xmlValidator = new XmlValidator()
      val result = xmlValidator.secureValidate(partialXml.getBytes)
//    TODO
//    result.isRight shouldBe true
    }
  }

  "XmlValidator unsafe parse" should {

    "return success (Right) if the bytes are XML that is valid and safe" in {
      val xmlValidator = new XmlValidator()
      val result = xmlValidator.insecureValidate(safeXml.getBytes)

      result.isRight shouldBe true
    }

    "return the expected response if a file cannot be securely parsed as valid XML" in {
      val xmlValidator = new XmlValidator()
      val result = xmlValidator.insecureValidate("Hello World".getBytes)

      result.isLeft shouldBe true

      val errorMsg: String = result.left.get
      errorMsg.contains("ParseError") shouldBe true
      errorMsg.contains("Content is not allowed in prolog") shouldBe true
    }

    "return the expected Left wrapped error message if bytes are unsafe XML" in {
      val xmlValidator = new XmlValidator()
      val result = xmlValidator.insecureValidate(unsafeXml.getBytes)

      result.isLeft shouldBe true

      val errorMsg: String = result.left.get
      errorMsg.contains("ParseError") shouldBe true
      errorMsg.contains("/does/not/exist (No such file or directory)") shouldBe true
    }
  }
}
