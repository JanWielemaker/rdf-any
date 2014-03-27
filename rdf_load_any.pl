/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(rdf_load_any,
	  [ rdf_load_any/1,		% +Input
	    rdf_load_any/2		% +Input, +Options
	  ]).
:- use_module(library(thread)).
:- use_module(library(uri)).
:- use_module(library(gensym)).
:- use_module(library(semweb/rdf_db)).
:- use_module(unpack).
:- use_module(rdf_detect).

%%	rdf_load_any(+Input) is det.
%%	rdf_load_any(+Input, +Options) is det.
%
%	Try to load RDF from Input.

rdf_load_any(Input) :-
	rdf_load_any(Input, []).
rdf_load_any(Input, Options) :-
	is_list(Input), !,
	concurrent_maplist(rdf_load_any_1(Options), Input).
rdf_load_any(Input, Options) :-
	rdf_load_any_1(Options, Input).

rdf_load_any_1(Options, Input) :-
	forall(unpack(Input, Stream, Location),
	       call_cleanup(load_stream(Stream, Location, Options),
			    close(Stream))).

load_stream(Stream, Location, Options) :-
	catch(load_stream_(Stream, Location, Options), E,
	      print_message(warning, E)).

load_stream_(Stream, Location, Options) :-
	location_base(Location, Base),
	(   (   file_name_extension(_, Ext, Base),
		Ext \== '',
		guess_format(Location.put(ext, Ext), DefFormat)
	    ;   guess_format(Location, DefFormat)
	    )
	->  Options1 = [format(DefFormat)|Options]
	;   Options1 = Options
	),
	rdf_guess_format(Stream, Format, Options1),
	rdf_load(stream(Stream),
		 [ format(Format),
		   base_uri(Base)
		 | Options
		 ]).

%%	location_base(+Location, -BaseURI) is det.
%
%	BaseURI describes the location from where the data is loaded.

location_base(Location, Base) :-
	location_base_base(Location, Base1),
	(   location_suffix(Location.data, Suffix)
	->  atomic_list_concat([Base1, Suffix], /, Base)
	;   Base = Base1
	).

location_base_base(Location, Location.get(url)) :- !.
location_base_base(Location, Base) :-
	uri_file_name(Base, Location.get(path)), !.
location_base_base(Location, Base) :-
	stream_property(Location.get(stream), file_name(FileName)), !,
	(   uri_is_global(FileName)
	->  Base = FileName
	;   uri_file_name(Base, FileName)
	).
location_base_base(_Location, Base) :-
	gensym('stream://', Base).

location_suffix([filter(_)|T], Suffix) :- !,
	location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix) :-
	_{name:data, format:raw} :< Archive, !,
	location_suffix(T, Suffix).
location_suffix([Archive|T], Suffix) :-
	(   location_suffix(T, Suffix0)
	->  atomic_list_concat([Archive.name, Suffix0], /, Suffix)
	;   Suffix = Archive.name
	).

guess_format(Location, Format) :-
	rdf_content_type(Location.get(content_type), Format), !.
guess_format(Location, Format) :-
	rdf_db:rdf_file_type(Location.get(ext), Format).

rdf_content_type('text/rdf',		  xml).
rdf_content_type('text/xml',		  xml).
rdf_content_type('text/rdf+xml',	  xml).
rdf_content_type('application/rdf+xml',	  xml).
rdf_content_type('application/x-turtle',  turtle).
rdf_content_type('application/turtle',	  turtle).
rdf_content_type('application/trig',	  trig).
rdf_content_type('application/n-triples', ntriples).
rdf_content_type('application/n-quads',   nquads).
rdf_content_type('text/turtle',		  turtle).
rdf_content_type('text/rdf+n3',		  turtle).	% Bit dubious
rdf_content_type('text/html',		  html).
rdf_content_type('application/xhtml+xml', xhtml).

