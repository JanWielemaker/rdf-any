:- module(rdf_detect,
	  [ rdf_content_type/3		% +Stream, -ContentType, +Options
	  ]).
:- use_module(library(sgml)).
:- use_module(library(memfile)).
:- use_module(library(option)).
:- use_module(library(dcg/basics)).

/** <module> Detect RDF document format from stream content
*/

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
	(   string_codes(String, Codes),
	    phrase(rdf_content_type(ContentType), Codes, _)
	->  true
	;   open_binary_string_stream(String, Stream),
	    guess_xml_type(Stream, ContentType)
	).

rdf_content_type(ContentType) -->
	turtle_like(ContentType), !.

%%	turtle_like(-ContentType)// is semidet.
%
%	True if the start of the   input matches a turtle-like language.
%	There are four of them:
%
%	  1. Turtle
%	  2. TRiG
%	  3. ntriples
%	  4. nquads.
%
%	The first three can all be handled   by the turtle parser, so it
%	doesn't matter too much.

turtle_like(ContentType) -->
	blank, !, blanks,
	turtle_like(ContentType).
turtle_like(ContentType) -->
	"#", !, skip_line,
	turtle_like(ContentType).
turtle_like(turtle) -->			% or TRiG
	"@", icase_keyword(Keyword), {turtle_keyword(Keyword)}, !.
turtle_like(turtle) -->			% or TRiG
	"PREFIX", blank, !.
turtle_like(turtle) -->			% or TRiG
	"BASE", blank, !.
turtle_like(ntriples) -->
	"<http", ("s";""), "://", !.
turtle_like(turtle) -->
	"[", !.
turtle_like(turtle) -->
	"(", !.


turtle_keyword(base).
turtle_keyword(prefix).

%%	xml_like(-Dialect)//
%
%	True if the input looks like an xml/html document.

xml_like(Dialect) -->
	blank, !, blanks,
	xml_like(Dialect).
xml_like(xml) -->
	"<?xml", blank, !.


		 /*******************************
		 *	      READ XML		*
		 *******************************/

%%	xml_doctype(+Stream, -DocType) is semidet.
%
%	Parse a _repositional_ stream and get the  name of the first XML
%	element *and* demand that this   element defines XML namespaces.
%	Fails if the document is illegal XML before the first element.
%
%	Note that it is not  possible   to  define valid RDF/XML without
%	namespaces, while it is not possible  to define a valid absolute
%	Turtle URI (using <URI>) with a valid xmlns declaration.

xml_doctype(Stream, DocType) :-
	catch(setup_call_cleanup(
		  make_parser(Stream, Parser, State),
		  sgml_parse(Parser,
			     [ source(Stream),
			       max_errors(1),
			       syntax_errors(quiet),
			       call(begin, on_begin),
			       call(cdata, on_cdata)
			     ]),
		  cleanup_parser(Stream, Parser, State)),
	      E, true),
	nonvar(E),
	E = tag(Dialect, DocType, Attributes),
	writeln(Dialect-Attributes).

make_parser(Stream, Parser, state(Pos)) :-
	stream_property(Stream, position(Pos)),
	new_sgml_parser(Parser, []).

cleanup_parser(Stream, Parser, state(Pos)) :-
	free_sgml_parser(Parser),
	set_stream_position(Stream, Pos).

on_begin(Tag, Attributes, Parser) :-
	get_sgml_parser(Parser, dialect(Dialect)),
	throw(tag(Dialect, Tag, Attributes)).

on_cdata(_CDATA, _Parser) :-
	throw(error(cdata)).


		 /*******************************
		 *	    DCG BASICS		*
		 *******************************/

skip_line --> [0'\n], !, skip_line.
skip_line --> [_], skip_line.

icase_keyword(Keyword) -->
	alpha_to_lower(H),
	alpha_to_lowers(T),
	{ atom_codes(Keyword, [H|T])
	}.

alpha_to_lowers([H|T]) -->
	alpha_to_lower(H),
	alpha_to_lowers(T).

%%	open_binary_string_stream(+String, -Stream) is det.
%
%	True when Stream is  a  binary   stream  holding  the context of
%	String.

open_binary_string_stream(String, Stream) :-
	new_memory_file(MF),
	setup_call_cleanup(
	    open_memory_file(MF, write, Out, [encoding(octet)]),
	    format(Out, '~s', [String]),
	    close(Out)),
	open_memory_file(MF, read, Stream,
			 [ free_on_close(true),
			   encoding(octet)
			 ]).
