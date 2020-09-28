:- ensure_loaded('../../interpreter/readylog.pl').
%:- ensure_loaded('processed_agent.pl').


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

prim_fluent(loc(B)) :- block(B).
initial_val(loc(a), table).
initial_val(loc(b), table).
initial_val(loc(c), a).

prim_action(stack(X, Y)) :-
	block(X), object(Y), X \= Y.

poss(stack(X, Y),
	not(or([
		loc(X) = Y,
		some(n,	loc(n) = X),
		some(m, loc(m) = Y)
	]))
) :-
	block(X), block(Y), X \= Y
.

poss(stack(X, table),
	not(or([
		loc(X) = table,
		some(n, loc(n) = X)
	]))
) :-
	block(X)
.

causes_val(stack(X, Y), loc(X), Y, true).

execute(stack(X, Y), _, H) :-
	printf('Stacking %w on %w.%n', [X, Y]); true. %*/


prim_action(print_status).
poss(print_status, true).
execute(print_status, _, H) :-
	has_val(loc(a), A_on, H),
	has_val(loc(b), B_on, H),
	has_val(loc(c), C_on, H),
	printf('a on %w, b on %w, c on %w%n', [A_on, B_on, C_on])
.


/* Reward function for goal */

function(goal_reward, V,
	lif(goal, V = 100, V = -1)
).


/* Do the planning */

proc(goal,
	and([
		loc(a) = table,
		loc(b) = a,
		loc(c) = b
	])
).

proc(main,
	solve(
		while(not(goal), [
			print_status,
			nondet(Any)
		]),
		8,
		goal_reward
	)
)  :-
	findall(stack(X, Y), prim_action(stack(X, Y)), Any)
.

