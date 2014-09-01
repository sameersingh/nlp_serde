package org.sameersingh.nlp_serde.readers

import org.sameersingh.nlp_serde.Document

/**
 * @author sameer
 * @since 9/1/14.
 */
class JsonReader extends Reader with DocPerFile {
  override def readDoc(name: String): Document = ???
}
