*parse_qs:string(QueryString, Spec)* checks a query string against a
specification, and translates it to a record. 

`QueryString` is a string (list of US-ASCII codes) that corresponds to a query as
described in rfc3986, section 3.4.

The result is either `{ok, Record, RemainingFields}` or `{error, Reason}`.

`RemainingFields` is a list `[{Name, Value}]` that contains the names and values
(as strings) of the fields in the query that don't occur in the Spec.

*parse_qs:create_spec(FileName, RecordName)* returns a `Spec` (as used by
parse_qs:string) or throws an error.

`FileName` is a string, `RecordName` is an atom. The file has to contain a record
declaration of a record with the specified name.

If the record declaration contains typed fields, the translation (as done by
parse_qs:string/2) will try to map the fields in the query to these types.

If the record declaration contains fields with a default value,
parse_qs:string/2 will put these values into the record if the query does not
provide a value.

If the record declaration contians fields that are not typed, but have a
default value, then parse_qs:string/2 will try to translate fields (if provided
in the query) to the type of the default value.

You can add information about the handling of the field as comments in the
record declaration (i.e. in the source code). You do this using tags of the
form "#Name ...". This idea was copied from EDoc (which uses "@Name ..." tags).

For the moment there are only some tags to indicate which fields are optional:
"#mandatory" and "#optional". Note that record fields are by default optional
if no default is provided, and mandatory if a default is provided.

In principle tags could be added to indicate things like field lengths etc.,
but this remains something to be done.

A simple example: in `parse_qs.erl` the record `person` is defined:

    -record(person,
      {name             :: string(),   %% #mandatory
    
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

`Spec = parse_qs:create_spec("parse_qs.erl", person).` will return a Spec. 

`parse_qs:string("name=Willem+de+Jong&gender=M&likes_erlang&project=erlsom&project=%20%3C%3E%25&other=blabla",
  Spec).` will return:

`{ok,{person,"Willem de Jong",undefined,'M',true,
                    ["erlsom"," <>%"],
                    "me@provider.com"},
            [{"other","blabla"}]}`


Below follow some details on the tagging, more or less copied from the EDoc
documentation.

A tag must be the first thing on a comment line, except for leading '%'
characters and whitespace. All the following text - including consecutive
comment lines - up until the end of the comment or the next tagged line, is
taken as the content of the tag.

Tags are associated with the field on the same line. In other words: tags for a
particular record field have to start on the exact same line that contains the
name of the field. Other constructs are ignored; e.g., in:

   -record(foo, {x =           % #mandatory "Bar" :: string(), y :: atom(),  
                    % #optional
      z}).          

the #mandatory tag is associated with the field x, and the #optional tag is
ignored.

Note that in a comment such as:

   % % #optional ...

the tag is ignored, because only the first '%' character is considered
"leading". This allows tags to be "commented out".

Text following the "#end" tag is always ignored. Use this to mark the end of
the previous tag, when necessary, as e.g. in:

   %% #optional true % #end % ----------------------------------
to avoid including the last "ruler" line in the #optional tag.
