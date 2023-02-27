/// SAX events that can be emitted by the parser.
///
/// These are based off of the events found in Erlang's `xmerl_sax_parser`.
///
pub type SaxEvent {
  /// Receive notification of the beginning of a document. The SAX parser will
  /// send this event only once before any other event callbacks. 
  StartDocument

  /// Receive notification of the end of a document. The SAX parser will send
  /// this event only once, and it will be the last event during the parse. 
  EndDocument

  /// Begin the scope of a prefix-URI Namespace mapping. Note that
  /// start/endPrefixMapping events are not guaranteed to be properly nested
  /// relative to each other: all startPrefixMapping events will occur
  /// immediately before the corresponding startElement event, and all
  /// endPrefixMapping events will occur immediately after the corresponding
  /// endElement event, but their order is not otherwise guaranteed. There will
  /// not be start/endPrefixMapping events for the "xml" prefix, since it is
  /// predeclared and immutable.
  StartPrefixMapping(prefix: String, uri: String)

  /// End the scope of a prefix-URI mapping.
  EndPrefixMapping(prefix: String)

  /// Receive notification of the beginning of an element. The Parser will send
  /// this event at the beginning of every element in the XML document; there
  /// will be a corresponding endElement event for every startElement event
  /// (even when the element is empty). All of the element's content will be
  /// reported, in order, before the corresponding endElement event.
  StartElement(
    uri: String,
    local_name: String,
    qualified_name: #(String, String),
    attributes: List(Attribute),
  )

  /// Receive notification of the end of an element. The SAX parser will send
  /// this event at the end of every element in the XML document; there will be
  /// a corresponding startElement event for every endElement event (even when
  /// the element is empty).
  EndElement(uri: String, local_name: String, qualified_name: #(String, String))

  /// Receive notification of character data. 
  Characters(String)

  /// Receive notification of ignorable whitespace in element content.
  IgnorableWhitespace(String)

  /// Receive notification of a processing instruction. The Parser will send
  /// this event once for each processing instruction found: note that
  /// processing instructions may occur before or after the main document
  /// element.
  ProcessingInstruction(target: String, data: String)

  /// Report an XML comment anywhere in the document (both inside and outside of
  /// the document element). 
  Comment(String)

  /// Report the start of a CDATA section. The contents of the CDATA section
  /// will be reported through the regular characters event. 
  StartCdata

  /// Report the end of a CDATA section. 
  EndCdata

  /// Report the start of DTD declarations, it's reporting the start of the
  /// DOCTYPE declaration. If the document has no DOCTYPE declaration, this
  /// event will not be sent.
  StartDtd(name: String, public_id: String, system_id: String)

  /// Report the end of DTD declarations, it's reporting the end of the DOCTYPE
  /// declaration. 
  EndDtd

  /// Report an element type declaration. The content model will consist of the
  /// string "EMPTY", the string "ANY", or a parenthesised group, optionally
  /// followed by an occurrence indicator. The model will be normalized so that
  /// all parameter entities are fully resolved and all whitespace is
  /// removed,and will include the enclosing parentheses. Other normalization
  /// (such as removing redundant parentheses or simplifying occurrence
  /// indicators) is at the discretion of the parser.
  ElementDecl(name: String, model: String)

  /// Report an attribute type declaration.
  AttributeDeclaration(
    element_name: String,
    attribute_name: String,
    type_: String,
    mode: String,
    value: String,
  )

  /// Report an internal entity declaration.
  InternalEntityDeclaration(name: String, value: String)

  /// Report a parsed external entity declaration.
  ExternalEntityDeclaration(name: String, public_id: String, system_id: String)

  /// Receive notification of an unparsed entity declaration event.
  UnparsedEntityDeclaration(
    name: String,
    public_id: String,
    system_id: String,
    notation_name: String,
  )

  /// Receive notification of a notation declaration event.
  NotationDeclaration(name: String, public_id: String, system_id: String)
}

pub type Attribute {
  Attribute(uri: String, prefix: String, name: String, value: String)
}

pub external fn sax(
  String,
  state,
  fn(state, Int, SaxEvent) -> state,
) -> Result(state, Nil) =
  "htmerl_ffi" "sax"
