#!/bin/bash

# ###################################################################### #
#  This program is free software; you can redistribute it and/or modify  #
#  it under the terms of the GNU General Public License as published by  #
#  the Free Software Foundation; version 2 of the License                #
# ###################################################################### #
#                                                                        
#           $Id$
#        author: Dennis Pannhausen
#   description: Shell script for evaluation of the relation between
#                training length and C4.5 classification accuracy
# last modified: $Date$
#            by: $Author$
#
# ###################################################################### #

usage()
{
cat <<EOF
-----------------------------------------------------------------------------
Evaluates the relationship between the training length and the accuracy
of the C4.5 classification.
The input consists of the C4.5 filestem.names and filestem.data
files.
This script takes the filestem filestem as main argument.

Usage: `basename $0` filestem [-stepSize] [-plotOnly]

options:
    -stepSize  : how many examples should be added to the training set of
                 C4.5 in each step

    -printOnly : if there is already an evaluation file filestem.eval,
                 this option skips the evaluation and only prints a graph
                 of the training curve.
                 The image is stored as filestem.eps
-----------------------------------------------------------------------------	    
EOF
}

if [[ "$1" = "usage" || "$1" = "-usage" || "$1" = "--usage" ]]
  then
   usage
   exit 1
fi
if [ "$#" -lt 1 ]
  then
   echo No filestem given!
   echo Usage: `basename $0` filestem [-stepSize] [-plotOnly]
   exit 1
fi
if [ "$#" -gt 3 ]
  then
   echo Too many arguments!
   echo Usage: `basename $0` filestem [-stepSize] [-plotOnly]
   exit 1
fi

if [ "$#" -eq 3 ]
  then
   if [[ "$3" = "-plotOnly" || "$3" = "plotOnly" ]]
   then
    echo plotOnly chosen
    plotOnly=1
  else
   echo Wrong arguments!
   echo Usage: `basename $0` filestem [-stepSize] [-plotOnly]
   exit 1
  fi
fi

if [[ ${plotOnly:-0} -eq 0 ]] # if plotOnly is false
  then
   if [ -f ./tmp.names ]
   then
    rm ./tmp.names
   fi
   if [ -f ./tmp.data ]
   then
    rm ./tmp.data
   fi
   if [ -f ./tmp.tree ]
   then
    rm ./tmp.tree
   fi
   if [ -f ./tmp.unpruned ]
   then
     rm ./tmp.unpruned
   fi

   cp $1.names "./tmp.names"

   numberOfExamples=$(awk 'END{ print NR}' $1.data)

   if [ "$#" -eq 2 ]
     then
      echo using stepSize of $2
      stepSize=$2
     else
      echo using default stepSize of 5
      stepSize=5
   fi


   if [ -f ./$1.eval ]
   then
    rm ./$1.eval
   fi

   firstRun="true"

   totalRuns=$(( $(( $numberOfExamples - 14 )) / $stepSize))
    
       totalRuns=$totalRuns || return $?
#       currentRun=1
#       width=${3:-25}
       width=25
#       mega=$(( 1024 * 1024 ))
       start=$(date +%s)

   echo "Evaluating $1..."
   echo "($numberOfExamples examples total)"

   for(( currentRun = 1; currentRun <= $totalRuns; currentRun += 1 ))
   do
         itemSetFirstLine=$(( 14 + $(( $currentRun * $stepSize )) ))

         if [ $(( $itemSetFirstLine + $stepSize )) -gt $numberOfExamples ]
         then
          itemSetLastLine=$numberOfExamples
         else   
          itemSetLastLine=$(( $itemSetFirstLine + $stepSize ))
         fi

#         echo writing examples from line $itemSetFirstLine to $itemSetLastLine [$numberOfExamples lines total]

         sed -n $itemSetFirstLine,$itemSetLastLine'p' $1.data >> tmp.data
       
         items=$(( $(($currentRun+1)) * $stepSize ))
#         echo $items items

         if [ $firstRun = "true" ]
         then
          # print the descriptions (-B4 grep 4 lines before), add "items" in front,
          # and comment out first 3 lines (sed).
          # add the item number in front of the first example line (line 5).
          c4.5 -f tmp | grep -B4 '<<' | sed -e '1,2s/.*/#\t &/g' \
                                            -e '3s/.*/# Items\t&/g' \
                                            -e "5s/.*/  $items \t&/g" \
                                            -e 's/(/ /g' \
                                            -e 's/)/ /g' > $1.eval
          firstRun="false"
         else
          c4.5 -f tmp | grep '<<' | sed -e "s/.*/  $items \t&/g" \
                                        -e 's/(/ /g' \
                                        -e 's/)/ /g' >> $1.eval
         fi

           ### progress bar ###
           [[ currentRun -gt totalRuns ]] && currentRun=$totalRuns

           # print truncated filename
           name=$(basename $1 | cut -b -20)
           printf "\r%-20s " $name 1>&2 

           # print percentage
           percent=$(( 100 * $currentRun / $totalRuns ))
           printf "%3d%% [" $percent 1>&2

           # print progress bar
           bar=$(( $width * $currentRun / $totalRuns ))
           for k in $(seq 1 $bar); do printf "=" 1>&2; done
           for k in $(seq $bar $(( $width-1 ))); do printf " " 1>&2; done

           # print number of current run
           printf "] %7s" "Run $currentRun/$totalRuns" 1>&2

           # print estimated time of arrival
           elapsed=$(( $(date +%s) - $start ))
           remain=$(( $totalRuns - $currentRun ))
           eta=$(( ($elapsed * $remain) / $currentRun + 1))
           if [[ $remain == 0 ]]; then eta=0; fi
           etamin=$(( $eta / 60 ))
           etasec=$(( $eta % 60 ))
           if [[ $eta > 0 ]]; then etastr="ETA"; else etastr="   "; fi
           printf "   %02d:%02d $etastr" $etamin $etasec 1>&2

   done

   echo ""
   echo "done :)"
   echo "results stored in $1.eval"
fi # end if [[ ${plotOnly:-0} -eq 0 ]]

if [ -f $1.eval ]
  then
   echo ""
   echo "Plotting graph for $1.eval..."
   gnuplot << EOF
   set terminal postscript eps color enhanced
   set output "$1.eps"
   set xlabel "Training Instances"
   set ylabel "Errors [in %]"
   set title "Learning Curve"
   plot "$1.eval" using 1:4 notitle w l
EOF
   echo "done :)"
   echo "graph stored in $1.eps"
  else
   echo "Error: File $1.eval does not exist!"
   exit 1
fi


if [ -f ./tmp.names ]
then
 rm ./tmp.names
fi
if [ -f ./tmp.data ]
then
 rm ./tmp.data
fi
if [ -f ./tmp.tree ]
then
 rm ./tmp.tree
fi
if [ -f ./tmp.unpruned ]
then
 rm ./tmp.unpruned
fi


