import java.io.{InputStream, InputStreamReader}

import javax.xml.stream.events.{DTD, EntityDeclaration, XMLEvent}
import javax.xml.stream.{XMLEventReader, XMLInputFactory}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object XMLSafetyChecker {

  def checkIfSafe(source: InputStream): Boolean =
    extractDtdDeclarations(source) match {
      case Success(dtdDeclarations) =>
        val entitySystemIds = for {
          declaration <- dtdDeclarations
          entity      <- declaration.getEntities.asScala.map(_.asInstanceOf[EntityDeclaration])
          systemId    <- Option(entity.getSystemId)
        } yield systemId
        entitySystemIds.isEmpty

      case Failure(e) =>
        true //Not an XML
    }

  private def extractDtdDeclarations(source: InputStream) = {
    val targetReader: InputStreamReader = new InputStreamReader(source)
    val streamReader: XMLEventReader    = buildXmlInputFactory().createXMLEventReader(targetReader)

    val eventStream = streamReader.asScala.toStream.map(_.asInstanceOf[XMLEvent])

    val dtdDeclarations = Try(
      eventStream
        .takeWhile(!_.isStartElement)
        .collect { case dtd: DTD => dtd })
    dtdDeclarations
  }

  private def buildXmlInputFactory(): XMLInputFactory = {
    val xmlInputFactory = XMLInputFactory.newFactory()
    xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, true)
    xmlInputFactory.setProperty("javax.xml.stream.isSupportingExternalEntities", false)
    xmlInputFactory
  }
}
