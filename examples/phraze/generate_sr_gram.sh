#!/bin/bash

cat $1 | grep -e "entry" -e "<skillset>" -e "<entities>" | sed 's/<\/*entry>//g' \
	| sed 's/\t//g' \
	| awk 'BEGIN {toggle=0; \
	printf "#JSGF V1.0;\ngrammar lexicon;\n\n<undefined> = something"} \
	/<entities>/ {toggle=1; printf ";\n<noun> = smurf"; next} \
	/<skillset>/ {toggle=2; printf ";\n<everb> = smurf"; next} \
	{a=$0; printf " | "a} \
	END {printf ";\n"}'
