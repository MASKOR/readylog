/* ***************************************************************************
 *  ,=³ .%%% %%%%%%%. .%%%%%.  .%%%%%.   %%%%%.%%. %. %%%%%%%% %% Rheinisch-
 * [==].%%%   %%   %% %%   %%  %%   %%   %%  %%'%%.%%.%% %% %% %% Westfälische
 *  //l%%%    %%   %% %%%%. ' %%       @ %%%%%' '%%%%%%% %% %%%%% Technische
 * [==]%%     %%|=%%   %%=>%  %%         %%  %%  '%%'%%% %% %% %% Hochschule
 * [==]%%%    %%   %% . '%%%% %%  '%%%   %%   %%  '% '%% %% %% %% Aachen
 * [==]'%%%   %%   %% %%   %%  %%   %%   http://kbsg.rwth-aachen.de/
 * o^^o '%%% %%%%%%%' '%%%%%'O '%%%%%'   Knowledge Based Systems Group
 * ***************************************************************************
 *
 *           $Id: language_processing.pl 2010-07-19 10:08
 *        author: Niklas Hoppe <niklas.hoppe@rwth-aachen.de>
 *   description: Language processing in the Maze domain.
 *
 * ************************************************************************ */

:- write(" --> loading language_processing.pl ... \n").

pue(U) :- process_utterance_external(U,_).
pue(U,E) :- process_utterance_external(U,E).
%process_utterance_external("quit",[say('quitting')]).
%process_utterance_external(U,[
%				processed_utterance,
%				waitForUtterance(Utterance),
%				?( process_utterance(Utterance) = AS ),
%				AS
%			]) :- split_string(U," ","",Phrase), writeln(Phrase), parse_utterance(Essence,Phrase,[]), writeln(Essence).
process_utterance_external(Utterance,Essence) :- split_string(Utterance," ","",Phrase), parse_utterance(Essence,Phrase,[]).

split_essence_external([_|[Essence]], VerbPhrases) :- splee(Essence,VerbPhrases).

splee([and,[V1,V2]],[V1|V]) :- splee(V2,V).
splee(V,[V]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PHRASE STRUCTURE RULES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_utterance(E) --> salutation, utterance(E).
parse_utterance(E) --> utterance(E).

utterance([statement,E]) --> statement(E).
utterance([imperative,E]) --> imperative(E).
utterance([ynquestion,E]) --> ynquestion(E).
%utterance([whnsquestion,E]) --> whnsquestion(E).
%utterance([whsquestion,E]) --> whsquestion(E).

statement(E) --> np(_), vp(E).%, {append(E1,E2,E)}.
imperative(E) --> vp(E). 
ynquestion(E) --> aux(_), np(_), vp(E).%, {append(E1,E2,EPrime), append(EPrime,E3,E)}.
%whnsquestion(E) --> wh-np(E1), aux(E2), np(E3), vp(E4), 
%	{append(E1,E2,EPrime), append(E3,E4,EPrimePrime), append(EPrime,EPrimePrime,E)}. % wh-non-subject-question
%whsquestion(E) --> wh-np(E1), vp(E2), {append(E1,E2,E)}. % wh-subject-question

% noun phrase
np(E) --> pronoun(E).
np(E) --> proper-noun(E).

%np([E]) --> nominal(E1), pp(E2), {append(E1,E2,E)}. 
% leads to problems when parsing an utterance with two or objects

np([E]) --> determiner(E1), nominal(E2), {append(E1,E2,E)}.
np([E]) --> determiner(E1), np(E2), {append(E1,E2,E)}.
%np([E2,E1,E3]) --> np(E1), conjunction([E2]), np(E3).

% nominals
nominal(E) --> noun(E).
nominal(E) --> favor(E).
nominal(E) --> noun(E1), nominal(E2), {append(E1,E2,E)}.

% verb phrase
vp(E) --> vpPrime(E).
vp([E2,[E1,E3]]) --> vpPrime(E1), conjunction([E2]), vp(E3).%, {append(E1,E3,EPrime)}.

vpPrime(E) --> verb(E).
vpPrime([E1,[objects,E2]]) --> verb([E1]), obp(E2).
vpPrime(E) --> courtesy(_), vpPrime(E).%, {append(E1,E2,E)}.

% auxiliary verb
aux([[aux,E]]) --> verb(E).

% object phrase - selfmade
obp([[nil|E]]) --> np(E).
obp(E) --> pp(E).
obp(E) --> np(E1), obp(E2), {append([[nil|E1]],E2,E)}.
obp(E) --> np(E1), conjunction([_]), obp(E2), {append([[nil|E1]],E2,E)}.
obp(E) --> pp(E1), obp(E2), {append(E1,E2,E)}.
obp(E) --> pp(E1), conjunction([_]), obp(E2), {append(E1,E2,E)}.

% propositional phrase
pp([E]) --> prep(E1), np(E2), {append(E1,E2,E)}.

% verbs
verb(E) --> averb(E).
verb(E) --> everb(E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC LEXICON %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SALUTATIONS
salutation --> [""].
salutation --> ["caesar", ","].
salutation --> ["robot", ","].
salutation --> ["hey", ","].
salutation --> ["hey", "you", ","].

% WH-WORDS
wh-np([]) --> ["who"].
wh-np([]) --> ["where"].
wh-np([]) --> ["what"].
wh-np([]) --> ["which"].
wh-np([]) --> ["how"].
wh-np([]) --> ["why"].

% SPECIAL CASE FAVOR
favor([favor]) --> ["favor"].

% DETERMINERS
determiner([the]) --> ["the"].
determiner([this]) --> ["this"].
determiner([that]) --> ["that"].
determiner([these]) --> ["these"].
determiner([those]) --> ["those"].
% indefinite articles:
determiner([a]) --> ["a"].
determiner([an]) --> ["an"].
% possessive pronouns:
determiner([my]) --> ["my"].
determiner([your]) --> ["your"].
determiner([his]) --> ["his"].
determiner([her]) --> ["her"].
determiner([its]) --> ["its"].
determiner([our]) --> ["our"].
determiner([their]) --> ["their"].
% quantifiers:
determiner([few]) --> ["few"].
determiner([few]) --> ["a", "few"].
determiner([many]) --> ["many"].
determiner([much]) --> ["much"].
determiner([each]) --> ["each"].
determiner([every]) --> ["every"].
determiner([some]) --> ["some"].
determiner([any]) --> ["any"].
% numbers:
determiner([one]) --> ["one"].
determiner([two]) --> ["two"].
determiner([three]) --> ["three"].
% ordinals?

% PREPOSITIONS
prep([on]) --> ["on"].
prep([in]) --> ["in"].
prep([at]) --> ["at"].
prep([since]) --> ["since"].
prep([for]) --> ["for"].
prep([ago]) --> ["ago"].
prep([before]) --> ["before"].
prep([to]) --> ["to"].
prep([past]) --> ["past"].
prep([till]) --> ["till"].
prep([until]) --> ["until"].
prep([by]) --> ["by"].
prep([by]) --> ["by"].
prep([next]) --> ["next"].
prep([beside]) --> ["beside"].
prep([under]) --> ["under"].
prep([below]) --> ["below"].
prep([over]) --> ["over"].
prep([above]) --> ["above"].
prep([across]) --> ["across"].
prep([through]) --> ["through"].
prep([into]) --> ["into"].
prep([towards]) --> ["towards"].
prep([onto]) --> ["onto"].
prep([from]) --> ["from"].
prep([of]) --> ["of"].
prep([off]) --> ["off"].
prep([out]) --> ["out"].
prep([of]) --> ["of"].
prep([about]) --> ["about"].
prep([near]) --> ["near"].

% CONJUNCTIONS
conjunction([and]) --> [",", "and"].
conjunction([and]) --> ["and"].
conjunction([and]) --> ["and", "then"].
conjunction([or]) --> ["or"].
conjunction([or]) --> [",", "or"].
conjunction([]) --> [","].
% for the sake of completeness:
conjunction([so]) --> ["so"].
conjunction([but]) --> ["but"].
conjunction([nor]) --> ["nor"].
conjunction([for]) --> ["for"].
conjunction([yet]) --> ["yet"]. 

% PRONOUNS
pronoun([i]) --> ["i"].
pronoun([he]) --> ["he"].
pronoun([she]) --> ["she"].
pronoun([it]) --> ["it"].
pronoun([we]) --> ["we"].
pronoun([you]) --> ["you"].
pronoun([they]) --> ["they"].
pronoun([me]) --> ["me"].
pronoun([him]) --> ["him"].
pronoun([her]) --> ["her"].
pronoun([us]) --> ["us"].
pronoun([them]) --> ["them"].
pronoun([this]) --> ["this"].
pronoun([that]) --> ["that"].
pronoun([these]) --> ["these"].
pronoun([here]) --> ["here"].
pronoun([there]) --> ["there"].
pronoun([yourself]) --> ["yourself"].
pronoun([hisself]) --> ["hisself"].
pronoun([herself]) --> ["herself"].
pronoun([itself]) --> ["itself"].
pronoun([themselves]) --> ["themselves"].
pronoun([myself]) --> ["myself"].

% TODO: do we need reflexive pronouns?

% COURTESY/PLEASE
courtesy([]) --> ["please"].

% AUXILIARY VERBS
% TODO: do we need negation for requests?
%verb([cannot]) --> ["can't"].
%verb([cannot]) --> ["cannot"].
%verb([cannot]) --> ["can", "not"].
%verb([dont]) --> ["don't"].
%verb([dont]) --> ["do not"].
%verb([neednot]) --> ["need", "not"].
averb([can]) --> ["can"].
averb([do]) --> ["do"].
averb([need]) --> ["need"].
averb([prefer]) --> ["prefer"].
averb([want]) --> ["want"].
averb([would]) --> ["would"].
averb([could]) --> ["could"].
averb([like]) --> ["like"].
averb([be]) --> ["be"].
averb([are]) --> ["are"].
averb([am]) --> ["am"].
averb([has]) --> ["has"].
averb([is]) --> ["is"].
averb([may]) --> ["may"].
averb([might]) --> ["might"].
averb([must]) --> ["must"].
averb([ought]) --> ["ought"].
averb([shall]) --> ["shall"].
averb([should]) --> ["should"].
averb([will]) --> ["will"].
averb([have]) --> ["have"].
averb([has]) --> ["has"].
averb([got]) --> ["got"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXTENDED LEXICON %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(lexicon_extension).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- write(" <-- loading language_processing.pl done.\n").