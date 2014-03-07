:- module(rdf_detect,
	  [ rdf_content_type/3		% +Stream, -ContentType, +Options
	  ]).
:- use_module(library(option)).
:- use_module(library(dcg/basics)).

%%	rdf_content_type(+Stream, -ContentType, +Options) is semidet.
%
%	True when Stream is  thought  to   contain  RDF  data  using the
%	indicated content type.  Options processed:
%
%	  - look_ahead(+Bytes)
%	  Look ahead the indicated amount

rdf_content_type(Stream, ContentType, Options) :-
	option(look_ahead(Bytes), Options, 1000),
	peek_string(Stream, Bytes, String),
	string_codes(String, Codes),
	phrase(rdf_content_type(ContentType), Codes, _).


rdf_content_type(ContentType) -->
	xml_prolog,
	xml_begin(Element).


		 /*******************************
		 *	   XML RECOGNISER	*
		 *******************************/

%%	xml_prolog//
%
%	Recognise an XML prologue.
%
%	@see http://www.w3.org/TR/xml/

xml_prolog -->
	opt_xml_decl,
	xml_miscs,
	(   xml_doctype
	->  xml_miscs
	;   []
	).

s --> [0x20].
s --> [0x09].
s --> [0x0d].
s --> [0x0a].
