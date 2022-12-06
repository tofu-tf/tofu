package tofu.logging

import tethys.commons.RawJson

package object json {
  type JsonEntry = (String, RawJson)

  object instances extends JsonEntryReader
}
