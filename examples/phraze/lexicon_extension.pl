:- write(" --> loading lexicon extension ...\n").

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXTENDED LEXICON %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% undefined objects -> nouns
undefined --> ["something"].
undefined --> ["somewhere"].
undefined --> ["someplace"].

% objects -> (proper) nouns
noun(["living_room"]) --> ["living","room"].
noun(["kitchen"]) --> ["kitchen"].
noun(["hall"]) --> ["hall"].
noun(["hallway"]) --> ["hallway"].
noun(["corridor"]) --> ["corridor"].
noun(["bedroom"]) --> ["bedroom"].
noun(["bath"]) --> ["bath"].
noun(["bathroom"]) --> ["bathroom"].
noun(["bath_room"]) --> ["bath","room"].
noun(["gold"]) --> ["gold"].
noun(["treasure"]) --> ["treasure"].

% skills/actions -> verb
everb(["scram"]) --> ["scram"].
everb(["go"]) --> ["go"].
everb(["move"]) --> ["move"].
everb(["fetch"]) --> ["fetch"].
everb(["bring"]) --> ["bring"].
everb(["collect"]) --> ["collect"].
everb(["transport"]) --> ["transport"].
everb(["move"]) --> ["move"].
everb(["bring"]) --> ["bring"].
everb(["go"]) --> ["go"].
everb(["move"]) --> ["move"].
everb(["navigate"]) --> ["navigate"].
everb(["walk"]) --> ["walk"].
everb(["drive"]) --> ["drive"].
everb(["run"]) --> ["run"].

:- write(" <-- loading lexicon extension done.\n").
