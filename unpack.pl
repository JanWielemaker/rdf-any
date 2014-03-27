:- module(unpack,
	  [ unpack/3			% +Spec, -Stream, -Location
	  ]).
:- use_module(library(http/http_open)).
:- use_module(library(archive)).
:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(apply)).

/** <module> Unpack encoded and archived material
*/

%%	unpack(+Spec, -Stream, -Location) is nondet.
%
%	Provide access to plain data elements in   a  stream that may be
%	compressed and/or contain (nested) archives.   For  each element
%	found, it succeeds with  Stream  unified   to  a  binary  stream
%	associated with the data  and  Location   is  a  dict containing
%	information about the  applied  filtering   and  possibly  other
%	meta-data available about the element.
%
%	Stream must be closed, but  it  is   not  required  to  read all
%	content from the stream.  Committing   before  all  elements are
%	exhausted is allowed and will cause   all allocated resources to
%	be reclaimed.
%
%	@arg	Location is a dict.  The tag indicates the type of Spec and
%		is currently one of =stream=, =file= or =url=. For URLs,
%		the keys =content_type=, =content_length= and
%		=last_modified= may be available.
%
%		The dict contains a key =data=, holding a list that
%		describes the decoding pipeline used for the data
%		element. Elements of this list are:
%
%		  - filter(Filter)
%		  The indicated content filter was applied.
%		  - archive_entry{}, where the keys provide all
%		  solutions of archive_header_property/2 and the
%		  key =name= provides the name of the archive entry.
%
%	@arg	Spec is a stream, URL or file name. If Spec is a stream,
%		it is _not_ closed after processing.

unpack(Spec, Stream, Location) :-
	open_input(Spec, In, Meta, Close),
	Location = Meta.put(data, Data),
	call_cleanup(content(In, Stream, Data), Close).


open_input(stream(In), In, stream{stream:In}, true).
open_input(In, In, stream{stream:In}, true) :-
	is_stream(In), !.
open_input(URL, In, Meta, close(In)) :-
	uri_components(URL, Components),
	uri_data(scheme, Components, Scheme),
	nonvar(Scheme),
	(   http_scheme(Scheme)
	->  rdf_extra_headers(Extra),
	    http_open(URL, In,
		      [ header(content_type, ContentType),
			header(content_length, ContentLength),
			header(last_modified, ModifiedText)
		      | Extra
		      ]),
	    url_meta_pairs([ content_type=ContentType,
			     content_length=ContentLength,
			     last_modified=ModifiedText
			   ], Pairs),
	    dict_pairs(Meta, url, [url-URL|Pairs])
	;   Scheme == file
	->  uri_file_name(URL, File),
	    Meta = file{path:File},
	    open(File, read, In, [type(binary)])
	).
open_input(URL, In, file{path:File}, close(In)) :-
	uri_file_name(URL, File), !,
	open(File, In, [type(binary)]).
open_input(Spec, In, file{path:Path}, close(In)) :-
	compound(Spec), !,
	absolute_file_name(Spec, Path, [access(read)]),
	open(Path, read, In, [type(binary)]).
open_input(File, In, file{path:File}, close(In)) :-
	exists_file(File),
	open(File, read, In, [type(binary)]).

http_scheme(http).
http_scheme(https).

url_meta_pairs([], []).
url_meta_pairs([_=''|T0], T) :- !,
	url_meta_pairs(T0, T).
url_meta_pairs([content_length=Atom|T0], [content_length-Bytes|T]) :-
	atom_number(Atom, Bytes), !,
	url_meta_pairs(T0, T).
url_meta_pairs([Name=Value|T0], [Name-Value|T]) :- !,
	url_meta_pairs(T0, T).



%%	content(+Stream, -SubStream, -PipeLine) is nondet.
%
%	True when SubStream is a raw content   stream for data in Stream
%	and PipeLine describes the location of the data.
%
%	@arg	PipeLine is a list of applied filters and archive selection
%		operations.  Elements take the form
%
%		  - filter(Name)
%		  - archive(Member, Format)

content(In, Entry, PipeLine) :-
	content(In, Entry, PipeLine, []).

content(In, Entry, PipeLine, PipeTail) :-
	setup_call_cleanup(
	    archive_open(stream(In), Ar, [format(all),format(raw)]),
	    archive_content(Ar, Entry, PipeLine, PipeTail),
	    archive_close(Ar)).

archive_content(Ar, Entry, PipeLine, PipeTail) :-
	archive_property(Ar, filter(Filters)),
	maplist(wrap_filter, Filters, PipeElements),
	append(PipeElements, RestPipe, PipeLine),
	repeat,
	(   archive_next_header(Ar, Name)
	->  findall(P, archive_header_property(Ar, P), Pl),
	    dict_create(Pipe, archive_entry, [name(Name)|Pl]),
	    (   Pipe.filetype == file
	    ->	archive_open_entry(Ar, Entry0),
		(   Name == data, Pipe.format == raw
		->  !, RestPipe = PipeTail,
		    Entry = Entry0
		;   RestPipe = [ Pipe | RestPipe1 ],
		    call_cleanup(content(Entry0, Entry, RestPipe1, PipeTail),
				 close(Entry0))
		)
	    ;	fail
	    )
	;   !,
	    fail
	).

wrap_filter(Filter, filter(Filter)).

		 /*******************************
		 *	    HTTP SUPPORT	*
		 *******************************/

:- public ssl_verify/5.

rdf_extra_headers(
        [ request_header('Accept' = 'application/x-turtle, \c
                                     application/turtle, \c
                                     application/trig, \c
                                     text/turtle; q=0.9, \c
                                     application/rdf+xml, \c
                                     text/rdf+xml; q=0.8, \c
                                     */*; q=0.1'),
          cert_verify_hook(ssl_verify)
        ]).

%%      ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%       Currently we accept  all  certificates.

ssl_verify(_SSL,
           _ProblemCertificate, _AllCertificates, _FirstCertificate,
           _Error).

