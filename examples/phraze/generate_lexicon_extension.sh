#!/bin/bash

cat $1 | grep -e "entry" -e "<skillset>" | sed 's/<\/*entry>//g' \
	| sed 's/\t//g' \
	| awk 'BEGIN {toggle=0; \
	print ":- write(\" --> loading lexicon extension ...\\n\").\n\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXTENDED LEXICON %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n% objects -> (proper) nouns"} \
	/skillset/ {toggle=1; print "\n% skills/actions -> verb"; next} toggle==0 {a=$0; b=$0; gsub(" ","_",a); gsub(" ","\",\"",b); print "noun(["a"]) --> [\""b"\"]."} toggle==1 {a=$0; b=$0; gsub(" ","_",a); gsub(" ","\",\"",b); print "everb(["a"]) --> [\""b"\"]."} END {print "\n:- write(\" <-- loading lexicon extension done.\\n\")."}'
