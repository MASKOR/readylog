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

Usage: `basename $0` filestem [-stepSize] [-testNum] [-plotOnly]

options:
    -stepSize : how many examples should be added to the training set of
                C4.5 in each step (default 5)

    -testNum  : every testNum instance should be kept apart in a test set
                (default 3)

    -plotOnly : if there is already an evaluation file filestem.eval,
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
   usage
   exit 1
fi

if [ "$#" -gt 4 ]
  then
   echo Too many arguments!
   usage
   exit 1
fi

if [ "$#" -eq 4 ]
  then
   if [[ "$4" = "-plotOnly" || "$4" = "plotOnly" ]]
   then
    echo plotOnly chosen
    plotOnly=1
   else
    echo Wrong arguments!
   usage
   exit 1
  fi
fi

if [ "$#" -gt 2 ]
  then
   if [ "$3" -lt 0 ]
   then
    echo Parameter testNum has to be greater than 0!
    usage
    exit 1
   fi
fi

if [ "$#" -gt 2 ]
  then
   echo "keeping apart a test set of (1/"$3")"
   testSpacing=$3
  else
   echo "keeping apart a test set of (1/3)"
   testSpacing=3
fi

if [[ ${plotOnly:-0} -eq 0 ]] # if plotOnly is false
  then
   if [ -f ./tmp.names ]
   then
    rm ./tmp.names
   fi
   if [ -f ./tmp.all ]
   then
    rm ./tmp.all
   fi
   if [ -f ./tmp.test ]
   then
    rm ./tmp.test
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

   # Number of (training + test) data instances
   # Test data will be extracted from $1.data and will be kept apart
   numberOfAllInstances=$(( $(awk 'END{ print NR}' $1.data) - 13 ))
   echo "numberofAllInstances: $numberOfAllInstances"
   testRatio=`echo "scale=2; 1/$testSpacing" | bc`
   echo "testRatio: $testRatio"
   testSetSizeFloat=`echo "scale=0; $testRatio*$numberOfAllInstances" | bc`
   # round to a pseudo-integer
   testSetSize=`printf "%.0f\n" $testSetSizeFloat`
   echo "testSetSize: $testSetSize"
   # Number of training instances
   numberOfExamples=$(( $numberOfAllInstances - $testSetSize ))
   echo "numberofExamples: $numberOfExamples"

   if [ "$#" -gt 1 ]
     then
      echo using stepSize of $2
      stepSize=$2
     else
      echo using default stepSize of 5
      stepSize=5
   fi


   if [ -f ./$1.eval.train ]
   then
    rm ./$1.eval.train
   fi
   if [ -f ./$1.eval.test ]
   then
    rm ./$1.eval.test
   fi

   firstRun="true"

   totalRunsFloat=`echo "scale=2; $numberOfAllInstances / $stepSize" | bc`
#   echo "totalRunsFloat: $totalRunsFloat"
   # round up to a pseudo-integer
   totalRuns=`perl -le 'print int(shift()+0.9999)' $totalRunsFloat`
#   echo "totalRuns: $totalRuns"
    
#   totalRuns=$totalRuns || return $?

#   currentRun=1
#   width=${3:-25}
   width=25
#   mega=$(( 1024 * 1024 ))
   start=$(date +%s)

   echo "Evaluating $1..."
   echo "($numberOfExamples training examples total)"

   for(( currentRun = 1; currentRun <= $totalRuns; currentRun += 1 ))
   do
         itemSetFirstLine=$(( 14 + $(( $(( $currentRun - 1 )) * $stepSize )) ))

         if [ $(( $itemSetFirstLine + $stepSize - 1 )) -gt $(( 13 + $numberOfAllInstances )) ]
         then
          itemSetLastLine=$(( 13 + $numberOfAllInstances ))
         else   
          itemSetLastLine=$(( $itemSetFirstLine + $stepSize - 1 ))
         fi

#         echo "adding examples from line $itemSetFirstLine to $itemSetLastLine [$numberOfAllInstances lines total]"

         sed -n $itemSetFirstLine,$itemSetLastLine'p' $1.data >> tmp.all
         # keep every $testSpacing (default 3rd) instance as test data
         sed -n '0~'$testSpacing'p' tmp.all > tmp.test
         # store the other instances as training data
         sed '0~'$testSpacing'd' tmp.all > tmp.data
#         sed -n $itemSetFirstLine,$itemSetLastLine'p' $1.data >> tmp.data
#         items=$(( $(($currentRun+1)) * $stepSize ))
         trainingItems=$(awk 'END{ print NR}' tmp.data)
#         testItems=$(awk 'END{ print NR}' tmp.test)
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
                                            -e 's/)/ /g' > $1.eval.train
          sed -n '1,4p' $1.eval.train > $1.eval.test

#          firstRun="false"
         else
           # store data for both the training set and the test set in
           # a temporary file
           c4.5 -u -f tmp | grep '<<' | sed -e "1s/.*/  $trainingItems \t&/g" \
                                            -e "2s/.*/  $trainingItems \t&/g" \
                                            -e 's/(/ /g' \
                                            -e 's/)/ /g' > $1.eval.tmp
           # split the data up
           sed -n '1p' $1.eval.tmp >> $1.eval.train
           sed -n '2p' $1.eval.tmp >> $1.eval.test
           rm $1.eval.tmp
#           c4.5 -f tmp | grep '<<' | sed -e "s/.*/  $items \t&/g" \
#                                         -e 's/(/ /g' \
#                                         -e 's/)/ /g' >> $1.test
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
           # doesn't really work linearly...
#           remain=$(( $totalRuns - $currentRun ))
#           eta=$(( ($elapsed * $remain) / $currentRun + 1))
#           firstRun="false"
           # Jobs are taking much longer when the data set grows.
           # Thus, we could estimate the remaining time by
           # supposing quadradic time complexity of C4.5:
#           remainLinear=$(( $totalRuns - $currentRun ))
#           remain=`echo "scale=2; $remainLinear ^ 2" | bc`
#           normaliser=`echo "scale=2; $(( currentRun + 1 )) ^ 2" | bc`
#           eta=$(( ($elapsed * $remain) / $normaliser ))
#           firstRun="false"
           # But we want a better estimate. So we spent some
           # time on inducing the biggest tree and then computing
           # the average job length:
           if [ $firstRun = "true" ]
           then 
              waitstr=" Please wait..."
              printf " $waitstr" 1>&2
              biggestJobStart=$(date +%s)
              c4.5 -u -f $1 > /dev/null
              biggestJobLength=$(( $(date +%s) - $biggestJobStart ))
              averageJobLength=$(( $(( $elapsed + $biggestJobLength )) / 2 ))
              # erase waitstr
              for(( i = 1; i <= 16; i += 1 ))
              do
                 printf "\b" 1>&2
              done
              firstRun="false"
           fi
#           echo "biggestJobLength=$biggestJobLength"
#           echo "averageJobLength=$averageJobLength"
#           echo "total Time estimation:$(( $averageJobLength*$totalRuns ))"
           totalLength=$(( $averageJobLength * $totalRuns ))
           eta=$(( $totalLength - $elapsed ))
#           echo "ETA: $eta"
           
           if [[ $remain == 0 ]]; then eta=0; fi
           etahours=$(( $eta / 3600 ))
           etamin=$(( $(( $eta / 60 )) % 60 ))
           etasec=$(( $eta % 60 ))
           if [[ $eta > 0 ]]; then etastr="ETA"; else etastr="   "; fi
           printf "   %02dh:%02dm:%02ds $etastr" $etahours $etamin $etasec 1>&2
   done

   echo ""
   echo "done :)"
   echo "results stored in $1.eval.train and $1.eval.test"
fi # end if [[ ${plotOnly:-0} -eq 0 ]]

if [ -f $1.eval.train ]
  then
   echo ""
   echo "Plotting graph for $1.eval.train..."
   gnuplot << EOF1
   set terminal postscript eps color enhanced
   set output "$1.train.eps"
   set xlabel "Training Instances"
   set xrange [ 0 : 18500 ]
   set xtics 500
   set mxtics 100
   set ylabel "Accuracy [in %]"
   set yrange [ 0 : 100 ]
   set ytics 10
   set mytics 5
   set title "Learning Curves of Univariate IPL Agent"
#  This was on the unpruned trees:
#   plot "$1.eval.train" using 1:(100-\$4) notitle w l
#  This is on the pruned trees:
   plot "$1.eval.train" using 1:(100-\$7) notitle w l
EOF1
   echo "done :)"
   echo "graph stored in $1.train.eps"
  else
   echo "Error: File $1.eval.train does not exist!"
   exit 1
fi
if [ -f $1.eval.test ]
  then
   echo ""
   echo "Plotting graph for $1.eval.test..."
   gnuplot << EOF2
   set terminal postscript eps color enhanced
   set output "$1.test.eps"
   set xlabel "Training Instances"
   set xrange [ 0 : 18500 ]
   set xtics 500
   set mxtics 100
   set ylabel "Accuracy [in %]"
   set yrange [ 0 : 100 ]
   set ytics 10
   set mytics 5
   set title "Learning Curves of Univariate IPL Agent"
#  This was on the unpruned trees:
#   plot "$1.eval.test" using 1:(100-\$4) notitle w l
#  This is on the pruned trees:
   plot "$1.eval.test" using 1:(100-\$7) notitle w l
EOF2
   echo "done :)"
   echo "graph stored in $1.test.eps"
  else
   echo "Error: File $1.eval.test does not exist!"
   exit 1
fi
if [[ '-f $1.eval.train' && '-f $1.eval.test' ]]
  then
   echo ""
   echo "Plotting graph for $1.eval.combined..."
   gnuplot << EOF3
   set terminal postscript eps enhanced
   set key right bottom
   set output "$1.combined.eps"
   set xlabel "Training Instances"
   set xrange [ 0 : 18500 ]
   set xtics 2500
   set mxtics 10
   set ylabel "Accuracy [in %]"
   set yrange [ 0 : 100 ]
   set ytics 10
   set mytics 10
   set title "Learning Curves of Univariate IPL Agent"
   set line style 1 lt 6 lw 1
#  This was on the unpruned trees:
#   plot "$1.eval.train" using 1:(100-\$4) ti "on training data" with lines, \
#        "$1.eval.test" using 1:(100-\$4) ti "on unseen data" w l ls 1
#  This is on the pruned trees:
   plot "$1.eval.train" using 1:(100-\$7) ti "on training data" with lines, \
        "$1.eval.test" using 1:(100-\$7) ti "on unseen data" w l ls 1
EOF3
   echo "done :)"
   echo "graph stored in $1.combined.eps"
  else
   echo "Error: File $1.eval.train or $1.eval.test does not exist!"
   exit 1
fi


if [ -f ./tmp.names ]
then
 rm ./tmp.names
fi
#if [ -f ./tmp.data ]
#then
# rm ./tmp.data
#fi
if [ -f ./tmp.tree ]
then
 rm ./tmp.tree
fi
if [ -f ./tmp.unpruned ]
then
 rm ./tmp.unpruned
fi


