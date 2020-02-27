/** some nice utils for every day use
* by Christian Fritz
*/

set_textstyle( Color, Type) :-
	Head = "\033[",
	(
	  Color = black ->
	  P = "0"
	;
	  Color = red ->
	  P = "1"
	;
	  Color = green ->
	  P = "2"
	;
	  Color = yellow ->
	  P = "3"
	;
	  Color = blue ->
	  P = "4"
	;
	  Color = pink ->
	  P = "5"
	;
	  Color = cyan ->
	  P = "6"
	;
	  Color = white ->
	  P = "7"
	),
	(
	  Type = bold ->
	  T = "1"
	;
	  Type = invert ->
	  T = "7"
	;
	  Type = normal ->
	  T = "0"
	),
	printf("%w%w;3%wm", [Head,T,P]).
	
set_textcolor( Color ) :-
	(
	  Color = black ->
	  P = "\033[30m"
	;
	  Color = red ->
	  P = "\033[31m"
	;
	  Color = green ->
	  P = "\033[32m"
	;
	  Color = yellow ->
	  P = "\033[33m"
	;
	  Color = blue ->
	  P = "\033[34m"
	;
	  Color = pink ->
	  P = "\033[35m"
	;
	  Color = cyan ->
	  P = "\033[36m"
	;
	  Color = white ->
	  P = "\033[37m"
	;
	  Color = normal ->
	  P = "\033[0;39m"
	),
	printf("%w", [P]).


/* do colored output */
printColor( Color, Text, Args ) :-
	set_textcolor( Color ),
	printf(Text, Args),
	set_textcolor( normal),
	flush(output).
