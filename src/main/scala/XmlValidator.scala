/**
  * Created by joanna.pinto on 24/04/2018.
  */

import java.io.{ByteArrayInputStream, InputStreamReader}
import javax.xml.stream.{XMLInputFactory, XMLStreamReader}

import scala.util.{Failure, Success, Try}

class XmlValidator() {


  def secureValidate(fileBytes: Array[Byte]): Either[String, Unit] = {
    val targetReader: InputStreamReader = new InputStreamReader(new ByteArrayInputStream(fileBytes))

    val streamReader = getSecureXMLInputFactory().createXMLStreamReader(targetReader)
    val result = Try(readStreamUntilEntityExpansion(streamReader))

    targetReader.close()

    result match {
      case Success(_) => Right(())
      case Failure(e) => Left(e.getMessage)
    }
  }

  def insecureValidate(fileBytes: Array[Byte]): Either[String, Unit] = {
    val targetReader: InputStreamReader = new InputStreamReader(new ByteArrayInputStream(fileBytes))

    val streamReader = getVulnerableXMLInputFactory().createXMLStreamReader(targetReader)
    val result = Try(readStreamUntilEntityExpansion(streamReader))

    targetReader.close()

    result match {
      case Success(_) => Right(())
      case Failure(e) => Left(e.getMessage)
    }
  }

  private def getSecureXMLInputFactory(): XMLInputFactory = {
    val xmlInputFactory = XMLInputFactory.newFactory()
    xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, false)
    xmlInputFactory.setProperty("javax.xml.stream.isSupportingExternalEntities", false)

    xmlInputFactory
  }


  private def getVulnerableXMLInputFactory() = XMLInputFactory.newFactory

  private def readStreamUntilEntityExpansion(streamReader: XMLStreamReader) = {
    streamReader.next()
    streamReader.next()
    streamReader.next()
  }
}
