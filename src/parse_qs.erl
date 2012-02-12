-module(parse_qs).

-export([test/0, string/2, create_spec/2]).

% Parse a 'query string' (rfc3986, section3.4) and create a record from it. It
% can be used to create a record directly from information entered into an HTML
% form, which can then be inserted into an ets or mnesia table.
% 
% The information about the record (a specification of the fields and types) is
% passed as a "specification". This spec can be created from the record
% declaration. In order to allow specification of properties that cannot be
% specified in a standard erlang record definition, it is possible to add some
% information in the form of tags in comments.
%

% Note that comments have to start on the line that defines the field!
%
% The parser will consider it an error if the qs provides no value for a field that was declared 
% non optional (in a comment: "% #optional false" or "#mandatory"). Note that this is also the case if 
% the record declaration provides a default value.

% The result is the record plus any remaining field-value pairs 
% (or {error, Reason}).

% #rec_desc{} and #field{} describe how a query string can be 
% translated to a record.
%
% Note: these record definitions are also used for the test. The
% tags in the comments ("#optional" etc.) are examples of how the record 
% can be annotated.
% 
-record(field, {
  nm         :: atom(),      
  line_nr    :: integer(),   %% The line number in the file - used to deal
                             %% with the merging of tags in comments.
  position   :: integer(),   %% To make it easy to put the value in the right 
                             %% place
  type       :: atom(),
  optional   :: boolean(),
  is_list    :: boolean(),   %% true if the element may occur more than once
  default = undefined :: any(),
  comment    :: list()}).    %% intermediate use only - holds information
                             %% from the comments in the hrl file

-record(rec_desc, {
  nm         :: atom(),      %% the name of the record
  fields     :: [#field{}],
  nr         :: integer(),   %% The number of elements of the record 
                             %% (incl. first record identifier element)
  m_fields   :: [string()]   %% To make it easy to check that all 
                             %% mandatory fields have been provided 
}).

-define(space, 32).
-define(cr,    13).
-define(lf,    10).
-define(tab,   9).

%% whitespace consists of 'space', 'carriage return', 'line feed' or 'tab'
-define(is_whitespace(C), 
        C =:= ?space; C =:= ?cr ; C =:= ?lf; C =:= ?tab).

-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).

unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.


%% test stuff
%% The record below is used by the test function.
-record(person,
  {name             :: string(),   %% #optional false

                                   %% a blank line (as above) ends a tag.
   github_account,                 %% #optional
   gender           :: atom(),     % % #optional
           
                                   %% the extra space "comments out" the tag.
   likes_erlang     :: boolean(), 
   project          :: [string()], 
   email =                         %% #optional true
                                   %% #end 
                                   %% Note: the tag has to be on the same 
                                   %% line as the field name.
     "me@provider.com"}). 

test() ->
  %% translate the record definition included in this file (parse_qs.erl) to 
  %% a 'spec', and parse a sample query string.
  %%
  %% Assumes that parse_qs.erl is in the "logical place", see the documentation
  %% for file:find_src()
  case filename:find_src("parse_qs.erl") of
    {error, _Reason} ->
      io:format("input file (parse_qs.erl) not found~n");
    {SourceFile, _} ->
      Spec = create_spec(SourceFile ++ ".erl", person),
      QS = "name=Willem+de+Jong&gender=M&likes_erlang&project=erlsom&project=%20%3C%3E%25&other=blabla",
      R = string(QS, Spec),
      io:format("Result: ~p~n", [R])
  end.
  
%% parse a query string, returns {ok, Record, RemainingFields} or {error, Reason}

string(QueryString, Spec = #rec_desc{nr= NrOfElements, nm = Name, m_fields = Mandatory,
                              fields = Fields}) ->
  Record = setelement(1, erlang:make_tuple(NrOfElements, undefined), Name),
  case readFields(QueryString, Spec, Mandatory, Record, []) of
    {ok, Record2, Rest} -> 
      %% some post processing: list fields are set to [] if they are 'undefined', 
      %% reversed if they have a value, default values are set.
      {ok, lists:foldl(fun postProcesRecord/2, Record2, Fields), Rest};
    NotOk ->
      NotOk
  end.

readFields([], _Spec, [], Record, OtherFields) ->
  {ok, Record, OtherFields};
readFields([], _Spec, MissingFields, _Record, _OtherFields) ->
  {error, lists:flatten(io_lib:format("missing fields: ~p", [MissingFields]))}; 
readFields(QueryString, Spec = #rec_desc{fields = Fields}, ToDo, Record, OtherFields) ->
  {Field, Value, T} = readField(QueryString, []),
  try list_to_existing_atom(Field) of
    Atom ->
      %% Look for the field in the Spec
      case lists:keyfind(Atom, #field.nm, Fields) of
        #field{optional=Nillable, type=Type} = FieldSpec ->
          %% if it is mandatory, remove it from the ToDo list
          NewToDo = if 
                      not Nillable -> lists:delete(Atom, ToDo);
                      true -> ToDo
                    end,
          %% convert to the right type
          try convertValue(Value, Type) of
            Converted -> 
              readFields(T, Spec, NewToDo, insertValue(Converted, Record, FieldSpec), OtherFields)
          catch 
            _Type:_Error -> {error, lists:flatten(io_lib:format("invalid value (~p) for field ~p of type ~p", 
                    [Value, Atom, Type]))}
          end;
        false ->
          readFields(T, Spec, ToDo, Record, [{Field, Value} | OtherFields])
      end
  catch 
    %% if the atom doesn't exist, it is not a field of the record
    _Type:_Error ->
      readFields(T, Spec, ToDo, Record, [{Field, Value} | OtherFields])
  end.

insertValue(Value, Record, #field{position = Position, is_list = false}) ->
  setelement(Position, Record, Value);
insertValue(Value, Record, #field{position = Position, is_list = true}) ->
  case element(Position, Record) of
    undefined ->
      setelement(Position, Record, [Value]); 
    List ->
      setelement(Position, Record, [Value | List])
  end.

%% returns {{key, value}, Tail}
readField([$%, A, B |T], Acc) when ?IS_HEX(A), ?IS_HEX(B) ->
  readField(T, [unhexdigit(A) * 16 + unhexdigit(B) | Acc]); 
readField([$+|T], Acc) ->
  readField(T, [$\s | Acc]); 
readField([$=|T], Acc) ->
  %% end of field name, Value follows
  {Value, T2} = readValue(T, []),
  {lists:reverse(Acc), Value, T2};
readField([H|T], Acc) when H == $& orelse H == $; ->
  %% a field without value, this is interpeted as 'true'
  {lists:reverse(Acc), "true", T};
readField([], Acc) ->
  %% a field without value, this is interpeted as 'true'
  {lists:reverse(Acc), "true", []};
readField([H|T], Acc) ->
  readField(T, [H | Acc]).

readValue([$%, A, B |T], Acc) when ?IS_HEX(A), ?IS_HEX(B) ->
  readValue(T, [unhexdigit(A) * 16 + unhexdigit(B) | Acc]); 
readValue([$+|T], Acc) ->
  readValue(T, [$\s | Acc]); 
readValue([H|T], Acc) when H == $& orelse H == $ ->
  {lists:reverse(Acc), T};
readValue([], Acc) ->
  {lists:reverse(Acc), []};
readValue([H|T], Acc) ->
  readValue(T, [H|Acc]).

convertValue(Value, bool) ->
  case string:to_lower(Value) of
    "true" -> true;
    "false" -> false;
    _ -> throw({error,"wrong value for boolean"})
  end;
convertValue(Value, integer) ->
  list_to_integer(Value);
convertValue(Value, atom) ->
  list_to_existing_atom(Value);
convertValue(Value, string) ->
  Value.

postProcesRecord(#field{position = Position, is_list = true, default = Default}, Record) ->
  case element(Position, Record) of
    undefined ->
      setelement(Position, Record, Default);
    Value -> 
      setelement(Position, Record, lists:reverse(Value))
  end;
postProcesRecord(#field{position = Position, default = Default}, Record) ->
  case element(Position, Record) of
    undefined ->
      setelement(Position, Record, Default);
    _ -> 
      Record
  end.

%% Below follows the code that translates an erlang type specification 
%% to a spec for a query string 

create_spec(HrlFile, RecordName) ->
  create_spec(HrlFile, RecordName, []).

create_spec(HrlFile, RecordName, Options) ->
  case file:read_file(HrlFile) of
    {ok, Bin} ->
      compile(binary_to_list(Bin), RecordName, Options);
    Error ->
      Error
  end.

%% returns a #rec_desc{} (or throws an error)
compile(Hrl, RecordName, _Options) ->
  {ok, Tokens, _} = erl_scan:string(Hrl),
  Forms = splitForms(Tokens),
  ParsedForms = [erl_parse:parse_form(Form) || Form <- Forms],

  %% find the declaration of the record (RecordName)
  %% Returns the line on which the record decl. starts and 
  %% a list of fields.
  {_Line, Fields} = findRecord(ParsedForms, RecordName),
  TranslatedFields = translateFields(Fields),

  %% extract the comments
  % erl_comment_scan returns strings for consecutive lines in 1 list. This is
  % convenient, since the tags cannot continue beyond such a list.  In other
  % words, if there is a tag in one of the lines, it either ends when we find
  % another tag, or when the set of lines ends.
  Comments = erl_comment_scan:string(Hrl),
  Merged = mergeComments(TranslatedFields, Comments),
  Mandatory = [F#field.nm || F <- Merged, F#field.optional == false, F#field.default == undefined],
  #rec_desc{nm= RecordName, nr = length(Merged) + 1, fields = Merged, m_fields = Mandatory}.

scanTags(Comments) ->
  scanTags(Comments, []).

%% if a stripped comment starts with #, then it is a tag. 
scanTags([], Acc) ->
  lists:reverse(lists:flatten(Acc));
scanTags([Comment | T], Acc) ->
  Normalized = normalize(string:strip(Comment, left, $%)),
  case Normalized of
    [$# | _Rest] ->
      {Tag, T2} = scanTag(Normalized, T),
      scanTags(T2, [Tag | Acc]);
    _ ->
      scanTags(T, Acc)
  end.

% unknown tags are simply ignored.  Known tags may span multiple lines, and end
% when a new tag is encountered, or with the last line, or with an #end tag (or
% rather, the newline after it) (conform edoc).  note that #endblabla would
% also match.

scanTag("#end" ++ _Tail, _Lines) ->
  {[], []};
scanTag(Tag, Lines) ->
  {Keyword, AfterKeyword} = scanKeyword(Tag),
  {Value, Rest} = scanValue(Lines, [AfterKeyword]),
  {{Keyword, Value}, Rest}.

scanKeyword([$# | Rest]) ->
  scanKeyword(Rest, []).

scanKeyword([], Acc) ->
  {lists:reverse(Acc), []};
scanKeyword([X | Rest], Acc) when ?is_whitespace(X) ->
  {lists:reverse(Acc), Rest};
scanKeyword([X | Rest], Acc) ->
  scanKeyword(Rest, [X | Acc]).

scanValue([], Acc) ->
  {string:join(lists:reverse(Acc), " "), []};
scanValue([Comment| Tail] = Rest, Acc) ->
  Normalized = normalize(string:strip(Comment, left, $%)),
  case Normalized of
    [$# | _Rest] ->
      {string:join(lists:reverse(Acc), " "), Rest};
    _ ->
      scanValue(Tail, [Normalized | Acc])
  end.

%% Comments that are behind a field name (up to the next blank line or 
%% element) are appendend to the field. 
%% The comment has to start on the same line as the field definition!
mergeComments(Fields, Comments) ->
  [addComment(Field, Comments) || Field <- Fields].

addComment(#field{line_nr = Line} = Field, Comments) ->
  case lists:keysearch(Line, 1, Comments) of
    {value, {_, _, _, Comment}} ->
      processComments(Field, scanTags(Comment));
    _ ->
      Field
  end.

processComments(Field, Comments) ->
  lists:foldl(fun processComment/2, Field, Comments).

processComment({"optional", Val}, Field) -> 
  Field#field{optional = normalizeBool(Val)};
processComment({"mandatory", Val}, Field) -> 
  Field#field{optional = not normalizeBool(Val)};
processComment(_, Field) ->
  Field.

normalizeBool("true") -> true;
normalizeBool("false") -> false;
normalizeBool([]) -> true. %% if there is no value, the default is true.

splitForms(Tokens) ->
  splitForms(Tokens, [], []).
splitForms([{dot, Line}  | Tail], TokenAcc, FormAcc) ->
  splitForms(Tail, [], [lists:reverse([{dot, Line} | TokenAcc]) | FormAcc]);
splitForms([], [], FormAcc) ->
  lists:reverse(FormAcc);
splitForms([Token | Tail], TokenAcc, FormAcc) ->
  splitForms(Tail, [Token | TokenAcc], FormAcc).

findRecord([], _) ->
  throw({error, "record not found"});
findRecord([{ok, {attribute, Line, record, {Name, Fields}}} | _], Name) ->
  {Line, Fields};
findRecord([_|T], Name) ->
  findRecord(T, Name).


%% -record(field, {nm :: atom(), position :: integer(), type :: atom(), optional :: bool()}).
translateFields(Fields) ->
  %% start at 2, since record name is on position 1
  translateFields(Fields, [], 2).

translateFields([], Acc, _Pos) ->
  lists:reverse(Acc);
translateFields([{typed_record_field, {record_field, LineNr, {atom, _, Name}, Default}, Type} | T], Acc, Pos) ->
  {_, _, DefaultValue} = Default,
  translateFields(T, [translateField(LineNr, Name, Type, DefaultValue, Pos) | Acc], Pos + 1);
translateFields([{typed_record_field, {record_field, LineNr, {atom, _, Name}}, Type} | T], Acc, Pos) ->
  translateFields(T, [translateField(LineNr, Name, Type, undefined, Pos) | Acc], Pos + 1);
translateFields([{record_field, LineNr, {atom, _, Name}, Default} | T], Acc, Pos) ->
  {Type, _, DefaultValue} = Default,
  translateFields(T, [translateField(LineNr, Name, 
        {type, undefined, Type, []}, %% slightly hacky
        %% If the field is not typed, but a default is provided, the type of the 
        %% default is used.
        DefaultValue, Pos) | Acc], Pos + 1);
translateFields([{record_field, LineNr, {atom, _, Name}} | T], Acc, Pos) ->
  translateFields(T, [translateField(LineNr, Name, 
        {type, undefined, string, []}, %% slightly hacky
        %% If the field is not typed and no default is provided, the value
        %% is treated as a string.
        undefined, Pos) | Acc], Pos + 1).

translateField(LineNr, Name, Type, Default, Pos) ->
  Field = translateType(Type),
  %% Only a comment ("% #optional false") can be used to 
  %% make the field non-optional in the qs.
  Field#field{nm = Name, line_nr = LineNr, position = Pos, default = Default}.

% returns #field{}
% The field is optional if 'undefined' is an allowed value. In practice that is
% always the case, except if a default value is specified (then a value is
% mandatory in the record).  In that case, the qs_parser will put the default
% if no value is provided in the qs.  Note that the parser will thow an error
% if the qs provides no value and the field was declared non optional (in a
% comment: % #optional false), even if there is a default!
translateType({type, _, union, Alternatives}) ->
  FilterUndefined = fun({atom, _, undefined}) -> true;
                       (_) -> false
                    end, 
  FilterDefined = fun(X) -> not(FilterUndefined(X)) end,
  %% look for 'undefined' (and remove it)
  Nillable = lists:any(FilterUndefined, Alternatives),
  Alternatives2 = lists:filter(FilterDefined, Alternatives),
  %% now it can either be a single simple type, or a real choice between 2 or more record types
  case Alternatives2 of
    [{type, _, list, [{type, _, _SimpleType, []} = TheType]}] -> %% can occur more than once
      #field{optional = Nillable, type = translateType2(TheType), is_list= true};
    [{type, _, _SimpleType, []} = TheType] ->
      #field{optional = Nillable, type = translateType2(TheType), is_list= false}
  end;
translateType({type, _, _Type, []} = SimpleType) ->
  #field{optional = false, type = translateType2(SimpleType), is_list= false}.

translateType2({type, _, integer, []}) -> integer;
translateType2({type, _, atom, []}) -> atom;
translateType2({type, _, bool, []}) -> bool;
translateType2({type, _, boolean, []}) -> bool;
translateType2({type, _, string, []}) -> string.

normalize(String) ->
  %% for easy processing, replace all tabs and newlines by a space.
  NoTabs = [case C of 
     W when ?is_whitespace(W) ->
       ?space;
     _ ->
       C
   end ||  C <- String],
  string:strip(NoTabs).
