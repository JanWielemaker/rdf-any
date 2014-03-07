:- use_module(unpack).

show(Source) :-
	call_cleanup(findall(Pipe, content(In, _, Pipe), Pipes), Close),
	format('~q.~n', [Meta.put(data, Pipes)]).
