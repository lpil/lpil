-module(htmerl_ffi).

-export([sax/3]).

sax(Html, Initial, Fun) ->
    EventFun = fun(Event, LineNumber, State) ->
        Fun(State, LineNumber, convert_event(Event))
    end,
    case htmerl:sax(Html, [{event_fun, EventFun}, {user_state, Initial}]) of
        {ok, State, _Warnings} ->
            {ok, State};
        _ ->
            % TODO: refine errors
            {error, nil}
    end.

convert_attribute({Uri, Prefix, AttributeName, Value}) ->
    {attribute, Uri, Prefix, AttributeName, Value}.

convert_event(Event) ->
    case Event of
        startDocument ->
            start_document;

        endDocument ->
            end_document;

        {startPrefixMapping, Prefix, Uri} ->
            {start_prefix_mapping, Prefix, Uri};

        {endPrefixMapping, Prefix} ->
            {end_prefix_mapping, Prefix};

        {startElement, Uri, LocalName, QualifiedName, Attributes} ->
            Attributes2 = lists:map(fun convert_attribute/1, Attributes),
            {start_element, Uri, LocalName, QualifiedName, Attributes2};

        {endElement, Uri, LocalName, QualifiedName} ->
            {end_element, Uri, LocalName, QualifiedName};

        {characters, _} ->
            Event;

        {ignorableWhitespace, Content} ->
            {ignorable_whitespace, Content};

        {processingInstruction, Target, Data} ->
            {processing_instruction, Target, Data};

        {comment, Content} ->
            {comment, Content};

        startCDATA ->
            start_cdata;

        endCDATA ->
            end_cdata;

        {startDTD, Name, PublicId, SystemId} ->
            {start_dtd, Name, PublicId, SystemId};

        endDTD ->
            end_dtd;

        {startEntity, SysId} ->
            {start_entity, SysId};

        {endEntity, SysId} ->
            {end_entity, SysId};

        {elementDecl, Name, Model} ->
            {element_declaration, Name, Model};

        {attributeDecl, ElementName, AttributeName, Type, Mode, Value} ->
            {attribute_declaration, ElementName, AttributeName, Type, Mode, Value};

        {internalEntityDecl, Name, Value} ->
            {internal_entity_declaration, Name, Value};

        {externalEntityDecl, Name, PublicId, SystemId} ->
            {external_entity_declaration, Name, PublicId, SystemId};

        {unparsedEntityDecl, Name, PublicId, SystemId, Ndata} ->
            {unparsed_entity_declaration, Name, PublicId, SystemId, Ndata};

        {notationDecl, Name, PublicId, SystemId} ->
            {notation_declaration, Name, PublicId, SystemId}
    end.
