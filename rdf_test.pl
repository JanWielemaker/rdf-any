:- module(rdf_test, []).

:- use_module(library(http/http_open)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).

:- use_module(rdf_detect).

url('http://www.kanzaki.com/works/2011/stat/ra-void.ttl').

:- initialization(test).

test:-
  test1,
  test2,
  test3.

test1:-
  url(Url),
  test1(Url).

test1(Url):-
  setup_call_cleanup(
    http_open(Url, Stream, []),
    (
      rdf_guess_format(Stream, ContentType, []),
      rdf_load(stream(Stream), [format(ContentType)])
    ),
    close(Stream)
  ).

test2:-
  url(Url),
  test2(Url).

test2(Url):-
  setup_call_cleanup(
    http_open(Url, Stream, []),
    (
      rdf_guess_format(Stream, _, []),
      copy_stream_data(Stream, current_output)
    ),
    close(Stream)
  ).

test3:-
  url(Url),
  test3(Url).

test3(Url):-
  setup_call_cleanup(
    http_open(Url, Stream, []),
    rdf_load(stream(Stream), [format(turtle)]),
    close(Stream)
  ).

