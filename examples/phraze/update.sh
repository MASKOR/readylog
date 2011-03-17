#!/bin/bash
ONTOLOGY="maze-ontology"

./generate_readylog_taxonomy.sh $ONTOLOGY > taxonomy.readylog
./generate_lexicon_extension.sh $ONTOLOGY > lexicon_extension.pl
./generate_sr_gram.sh $ONTOLOGY > lexicon.gram
