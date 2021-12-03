package tofu.logging.json

import tethys._
import tethys.commons.RawJson
import tethys.jackson._
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, KeyReader, ReaderError}

import scala.annotation._
import scala.collection._

trait JsonEntryReader {

  implicit val listOfJsonEntries: JsonReader[List[JsonEntry]] =
    new JsonReader[List[JsonEntry]] {

      override def read(it: TokenIterator)(implicit fieldName: FieldName): List[JsonEntry] =
        if (it.currentToken().isObjectStart) recRead(it.next(), List.newBuilder)(fieldName)
        else ReaderError.wrongJson(s"Expected object start but found: ${it.currentToken()}")

      @tailrec
      private def recRead(it: TokenIterator, builder: mutable.Builder[JsonEntry, List[JsonEntry]])(
          fieldName: FieldName
      ): List[JsonEntry] =
        it.currentToken() match {
          case token if token.isObjectEnd =>
            it.nextToken()
            builder.result()
          case token if token.isFieldName =>
            val name          = it.fieldName()
            val nextFieldName = fieldName.appendFieldName(name)
            appendBuilder(it.next(), builder, KeyReader.stringKeyReader.read(name)(nextFieldName))(nextFieldName)
            recRead(it, builder)(fieldName)

          case token => ReaderError.wrongJson(s"Expect end of object or field name but '$token' found")(fieldName)
        }

      private def appendBuilder(it: TokenIterator, builder: mutable.Builder[JsonEntry, List[JsonEntry]], key: String)(
          implicit fieldName: FieldName
      ): Unit =
        builder += key -> RawJson.rawJsonReader.read(it)

    }
}
