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
noun([mug]) --> ["mug"].
noun([cup]) --> ["cup"].
noun([trophy]) --> ["trophy"].
noun([cup]) --> ["cup"].

% skills/actions -> verb
everb([scram]) --> ["scram"].
everb([go]) --> ["go"].
everb([move]) --> ["move"].
everb([scat]) --> ["scat"].
everb([fetch]) --> ["fetch"].
everb([bring]) --> ["bring"].
everb([collect]) --> ["collect"].
everb([transport]) --> ["transport"].
everb([move]) --> ["move"].
everb([transport]) --> ["transport"].
everb([move]) --> ["move"].
everb([bring]) --> ["bring"].
everb([go]) --> ["go"].
everb([move]) --> ["move"].
everb([navigate]) --> ["navigate"].
everb([walk]) --> ["walk"].
everb([drive]) --> ["drive"].
everb([run]) --> ["run"].
everb([shake]) --> ["shake"].

:- write(" <-- loading lexicon extension done.\n").
