:- ensure_loaded('../../interpreter/readylog/readylog.pl').
:- ensure_loaded('processed_agent.pl').


/* Boilerplate stuff */

wait_for_exog_occurs :- fail.
events_list([]).
param_cycletime(99999999).
ignore(_).
%:- toggle_dtdebug.
%:- set_event_handler(event_exogUpdate, ignore/1).


/* BAT */

blocks([a,b,c]).
block(X) :- blocks(L), member(X, L).
object(X) :- block(X); X = table.

prim_fluent(on(B)) :- block(B).
initial_val(on(a), table).
initial_val(on(b), table).
initial_val(on(c), a).

prim_fluent(tried(X, Y)) :-
	block(X), object(Y).
initial_val(tried(X, Y), false) :-
	block(X), object(Y).


prim_action(stack(X, Y)) :-
	block(X), object(Y), X \= Y.

poss(stack(X, Y),
	not(or([
		tried(X, Y),
		on(X) = Y,
		some(n,	on(n) = X),
		some(m, on(m) = Y)
	]))
) :-
	block(X), block(Y), X \= Y
.

poss(stack(X, table),
	not(or([
		tried(X, table),
		on(X) = table,
		some(n, on(n) = X)
	]))
) :-
	block(X)
.

causes_val(stack(X, Y), on(X), Y, true).
causes_val(stack(X, Y), tried(X, Y), true, true).

execute(stack(X, Y), _, H) :-
	printf('Stacking %w on %w.%n', [X, Y]); true. %*/


prim_action(print_status).
poss(print_status, true).
execute(print_status, _, H) :-
	has_val(on(a), A_on, H),
	has_val(on(b), B_on, H),
	has_val(on(c), C_on, H),
	printf('a on %w, b on %w, c on %w%n', [A_on, B_on, C_on])
.


/* Reward function for goal */

function(goal_reward, V,
	lif(goal, V = 100, V = 0)
).


/* Do the planning */

proc(goal,
	and([
		on(a) = table,
		on(b) = a,
		on(c) = b
	])
).

proc(main,
	solve(
/*		pickBest(Y, [a, table],
			stack(b, Y)
		),%*/
		while(not(goal), [
			print_status,
			nondet(Any)
		]),
	8,
	goal_reward
	)
)  :-
	findall(stack(X, Y), prim_action(stack(X, Y)), Any)
%*/
.

