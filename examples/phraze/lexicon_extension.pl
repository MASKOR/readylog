:- write(" --> loading lexicon extension ...\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXTENDED LEXICON %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% objects -> (proper) nouns
noun([something]) --> ["something"].
noun([some]) --> ["some"].
noun([any]) --> ["any"].
noun([object]) --> ["object"].
noun([thing]) --> ["thing"].
noun([anything]) --> ["anything"].
noun([somewhere]) --> ["somewhere"].
noun([anywhere]) --> ["anywhere"].
noun([place]) --> ["place"].
noun([location]) --> ["location"].
noun([kitchen]) --> ["kitchen"].
noun([bath]) --> ["bath"].
noun([bath_room]) --> ["bath","room"].
noun([cup]) --> ["cup"].
noun([mug]) --> ["mug"].

% skills/actions -> verb
everb([move]) --> ["move"].
everb([bring]) --> ["bring"].
everb([fetch]) --> ["fetch"].
everb([collect]) --> ["collect"].
everb([move]) --> ["move"].
everb([bring]) --> ["bring"].
everb([go]) --> ["go"].
everb([move]) --> ["move"].
everb([navigate]) --> ["navigate"].
everb([walk]) --> ["walk"].
everb([drive]) --> ["drive"].
everb([run]) --> ["run"].

:- write(" <-- loading lexicon extension done.\n").
