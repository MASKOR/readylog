#!/bin/bash

# ###################################################################### #
#  This program is free software; you can redistribute it and/or modify  #
#  it under the terms of the GNU General Public License as published by  #
#  the Free Software Foundation; version 2 of the License                #
# ###################################################################### #
#                                                                        
#           $Id$
#        author: Dennis Pannhausen
#   description: Shell script for evaluation of the decision tree
#                consultation success and the learned policy application
#                success of the Wumpus World agent.
# last modified: $Date:$
#            by: $Author:$
#
# ###################################################################### #

usage()
{
cat <<EOF
-----------------------------------------------------------------------------
Evaluates the consultation success and policy application success of the
Wumpus World agent by first plotting the relationship between the number of
total consultations and the ratio
(successful consultations/total consultations),
and second by plotting the relationship between the number of total
learned policy applications and the ratio
(successfully applied (complete) learned policies
 /total applied learned policies).

Usage: `basename $0`
-----------------------------------------------------------------------------	    
EOF
}

if [[ "$1" = "usage" || "$1" = "-usage" || "$1" = "--usage" ]]
  then
   usage
   exit 1
fi

if [ "$#" -gt 0 ]
  then
   echo Too many arguments!
   usage
   exit 1
fi

### single graphs ###

### univariate ###

if [ -f ./learned_policy.iplearn.uvar.seen ]
  then
   # The original data is incorrect -> infer the correct data from the already correct
   # values "total consultations", "successful consultations", and "failed applications".
   grep 'total consultations of learned policies.' learned_policy.iplearn.uvar.seen | \
        sed -e "s/.0 total consultations of learned policies.//g" > corrected.uvar.seen1 
   grep 'successful consultations of learned policies.' learned_policy.iplearn.uvar.seen | \
        sed -e "s/.0 successful consultations of learned policies.//g" > corrected.uvar.seen2 
   paste corrected.uvar.seen[12] > corrected.tmp
   # failed consultations
   awk '{x=$1-$2; print x}' corrected.tmp > corrected.uvar.seen3
   # ratio successful consultations/total consultations
   awk '{x=$2/$1; print x}' corrected.tmp > corrected.uvar.seen4
   rm corrected.tmp
   paste corrected.uvar.seen2 > corrected.tmp
   # total applications (= successful consultations)
   cat corrected.tmp > corrected.uvar.seen5
   rm corrected.tmp
   grep 'failed (complete) applications of learned policies.' learned_policy.iplearn.uvar.seen | \
        sed -e "s/.0 failed (complete) applications of learned policies.//g" > corrected.uvar.seen7
   paste corrected.uvar.seen[57] > corrected.tmp
   # successful applications (total applications - failed applications)
   awk '{x=$1-$2; print x}' corrected.tmp > corrected.uvar.seen6
   rm corrected.tmp
   paste corrected.uvar.seen[56] > corrected.tmp
   # ratio successful applications / total applications
   awk '{x=$2/$1; print x}' corrected.tmp > corrected.uvar.seen8
   rm corrected.tmp

   paste corrected.uvar.seen[12345678] > corrected.uvar.seen
   rm corrected.uvar.seen[12345678]

   echo ""
   echo "Plotting consultation success graph for learned_policy.iplearn.uvar.seen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "consultation_success_uvar_seen.eps"
   set xlabel "Number of Consultations"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful Consultations / Total Consultations"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Decision Tree Consultation Success in Seen Levels"
   plot "corrected.uvar.seen" using 1:4 ti "IPLearning (univariate)" with lines
EOF1
   echo "done :)"
   echo ""
   echo "Plotting application success graph for learned_policy.iplearn.uvar.seen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "application_success_uvar_seen.eps"
   set xlabel "Number of Applications"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful (Complete) Applications / Total Applications"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Learned Policy Application Success in Seen Levels"
   plot "corrected.uvar.seen" using 1:8 ti "IPLearning (univariate)" with lines
EOF2
   echo "done :)"
   echo "graphs stored in consultation_success_uvar_seen.eps and application_success_uvar_seen.eps"
  else
   echo "Error: File learned_policy.iplearn.uvar.seen does not exist!"
   exit 1
fi


if [ -f ./learned_policy.iplearn.uvar.unseen ]
  then
   # The original data is incorrect -> infer the correct data from the already correct
   # values "total consultations", "successful consultations", and "failed applications".
   grep 'total consultations of learned policies.' learned_policy.iplearn.uvar.unseen | \
        sed -e "s/.0 total consultations of learned policies.//g" > corrected.uvar.unseen1 
   grep 'successful consultations of learned policies.' learned_policy.iplearn.uvar.unseen | \
        sed -e "s/.0 successful consultations of learned policies.//g" > corrected.uvar.unseen2 
   paste corrected.uvar.unseen[12] > corrected.tmp
   # failed consultations
   awk '{x=$1-$2; print x}' corrected.tmp > corrected.uvar.unseen3
   # ratio successful consultations/total consultations
   awk '{x=$2/$1; print x}' corrected.tmp > corrected.uvar.unseen4
   rm corrected.tmp
   paste corrected.uvar.unseen2 > corrected.tmp
   # total applications (= successful consultations)
   cat corrected.tmp > corrected.uvar.unseen5
   rm corrected.tmp
   grep 'failed (complete) applications of learned policies.' learned_policy.iplearn.uvar.unseen | \
        sed -e "s/.0 failed (complete) applications of learned policies.//g" > corrected.uvar.unseen7
   paste corrected.uvar.unseen[57] > corrected.tmp
   # successful applications (total applications - failed applications)
   awk '{x=$1-$2; print x}' corrected.tmp > corrected.uvar.unseen6
   rm corrected.tmp
   paste corrected.uvar.unseen[56] > corrected.tmp
   # ratio successful applications / total applications
   awk '{x=$2/$1; print x}' corrected.tmp > corrected.uvar.unseen8
   rm corrected.tmp

   paste corrected.uvar.unseen[12345678] > corrected.uvar.unseen
   rm corrected.uvar.unseen[12345678]

   echo ""
   echo "Plotting consultation success graph for learned_policy.iplearn.uvar.unseen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "consultation_success_uvar_unseen.eps"
   set xlabel "Number of Consultations"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful Consultations / Total Consultations"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Decision Tree Consultation Success in Unseen Levels"
   plot "corrected.uvar.unseen" using 1:4 ti "IPLearning (univariate)" with lines
EOF1
   echo "done :)"
   echo ""
   echo "Plotting application success graph for learned_policy.iplearn.uvar.unseen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "application_success_uvar_unseen.eps"
   set xlabel "Number of Applications"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful (Complete) Applications / Total Applications"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Learned Policy Application Success in Unseen Levels"
   plot "corrected.uvar.unseen" using 1:8 ti "IPLearning (univariate)" with lines
EOF2
   echo "done :)"
   echo "graphs stored in consultation_success_uvar_unseen.eps and application_success_uvar_unseen.eps"
  else
   echo "Error: File learned_policy.iplearn.uvar.unseen does not exist!"
   exit 1
fi


### multivariate ###

if [ -f ./learned_policy.iplearn.mvar.seen ]
  then
   # The original data is incorrect -> infer the correct data from the already correct
   # values "total consultations", "successful consultations", and "failed applications".
   grep 'total consultations of learned policies.' learned_policy.iplearn.mvar.seen | \
        sed -e "s/.0 total consultations of learned policies.//g" > corrected.mvar.seen1 
   grep 'successful consultations of learned policies.' learned_policy.iplearn.mvar.seen | \
        sed -e "s/.0 successful consultations of learned policies.//g" > corrected.mvar.seen2 
   paste corrected.mvar.seen[12] > corrected.tmp
   # failed consultations
   awk '{x=$1-$2; print x}' corrected.tmp > corrected.mvar.seen3
   # ratio successful consultations/total consultations
   awk '{x=$2/$1; print x}' corrected.tmp > corrected.mvar.seen4
   rm corrected.tmp
   paste corrected.mvar.seen2 > corrected.tmp
   # total applications (= successful consultations)
   cat corrected.tmp > corrected.mvar.seen5
   rm corrected.tmp
   grep 'failed (complete) applications of learned policies.' learned_policy.iplearn.mvar.seen | \
        sed -e "s/.0 failed (complete) applications of learned policies.//g" > corrected.mvar.seen7
   paste corrected.mvar.seen[57] > corrected.tmp
   # successful applications (total applications - failed applications)
   awk '{x=$1-$2; print x}' corrected.tmp > corrected.mvar.seen6
   rm corrected.tmp
   paste corrected.mvar.seen[56] > corrected.tmp
   # ratio successful applications / total applications
   awk '{x=$2/$1; print x}' corrected.tmp > corrected.mvar.seen8
   rm corrected.tmp

   paste corrected.mvar.seen[12345678] > corrected.mvar.seen
   rm corrected.mvar.seen[12345678]

   echo ""
   echo "Plotting consultation success graph for learned_policy.iplearn.mvar.seen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "consultation_success_mvar_seen.eps"
   set xlabel "Number of Consultations"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful Consultations / Total Consultations"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Decision Tree Consultation Success in Seen Levels"
   plot "corrected.mvar.seen" using 1:4 ti "IPLearning (multivariate)" with lines
EOF1
   echo "done :)"
   echo ""
   echo "Plotting application success graph for learned_policy.iplearn.mvar.seen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "application_success_mvar_seen.eps"
   set xlabel "Number of Applications"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful (Complete) Applications / Total Applications"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Learned Policy Application Success in Seen Levels"
   plot "corrected.mvar.seen" using 1:8 ti "IPLearning (multivariate)" with lines
EOF2
   echo "done :)"
   echo "graphs stored in consultation_success_mvar_seen.eps and application_success_mvar_seen.eps"
  else
   echo "Error: File learned_policy.iplearn.mvar.seen does not exist!"
   exit 1
fi


if [ -f ./learned_policy.iplearn.mvar.unseen ]
  then
   # The original data is incorrect -> infer the correct data from the already correct
   # values "total consultations", "successful consultations", and "failed applications".
   grep 'total consultations of learned policies.' learned_policy.iplearn.mvar.unseen | \
        sed -e "s/.0 total consultations of learned policies.//g" > corrected.mvar.unseen1 
   grep 'successful consultations of learned policies.' learned_policy.iplearn.mvar.unseen | \
        sed -e "s/.0 successful consultations of learned policies.//g" > corrected.mvar.unseen2 
   paste corrected.mvar.unseen[12] > corrected.tmp
   # failed consultations
   awk '{x=$1-$2; print x}' corrected.tmp > corrected.mvar.unseen3
   # ratio successful consultations/total consultations
   awk '{x=$2/$1; print x}' corrected.tmp > corrected.mvar.unseen4
   rm corrected.tmp
   paste corrected.mvar.unseen2 > corrected.tmp
   # total applications (= successful consultations)
   cat corrected.tmp > corrected.mvar.unseen5
   rm corrected.tmp
   grep 'failed (complete) applications of learned policies.' learned_policy.iplearn.mvar.unseen | \
        sed -e "s/.0 failed (complete) applications of learned policies.//g" > corrected.mvar.unseen7
   paste corrected.mvar.unseen[57] > corrected.tmp
   # successful applications (total applications - failed applications)
   awk '{x=$1-$2; print x}' corrected.tmp > corrected.mvar.unseen6
   rm corrected.tmp
   paste corrected.mvar.unseen[56] > corrected.tmp
   # ratio successful applications / total applications
   awk '{x=$2/$1; print x}' corrected.tmp > corrected.mvar.unseen8
   rm corrected.tmp

   paste corrected.mvar.unseen[12345678] > corrected.mvar.unseen
   rm corrected.mvar.unseen[12345678]

   echo ""
   echo "Plotting consultation success graph for learned_policy.iplearn.mvar.unseen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "consultation_success_mvar_unseen.eps"
   set xlabel "Number of Consultations"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful Consultations / Total Consultations"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Decision Tree Consultation Success in Unseen Levels"
   plot "corrected.mvar.unseen" using 1:4 ti "IPLearning (multivariate)" with lines
EOF1
   if [ -f ./learned_policy.plot.iplearn.mvar.unseen.consultations ]
    then
     rm ./learned_policy.plot.iplearn.mvar.unseen.consultations
   fi
   echo "done :)"
   echo ""
   echo "Plotting application success graph for learned_policy.iplearn.mvar.unseen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "application_success_mvar_unseen.eps"
   set xlabel "Number of Applications"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful (Complete) Applications / Total Applications"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Learned Policy Application Success in Unseen Levels"
   plot "corrected.mvar.unseen" using 1:8 ti "IPLearning (multivariate)" with lines
EOF2
   echo "done :)"
   echo "graphs stored in consultation_success_mvar_unseen.eps and application_success_mvar_unseen.eps"
  else
   echo "Error: File learned_policy.iplearn.mvar.unseen does not exist!"
   exit 1
fi

### multiple graphs ###

### uvar vs. mvar ###

if [[ -f ./learned_policy.iplearn.uvar.seen && -f ./learned_policy.iplearn.mvar.seen ]]
  then
   echo ""
   echo "Plotting consultation success graph for learned_policy.iplearn.uvarVSmvar.seen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "consultation_success_uvarVSmvar_seen.eps"
   set xlabel "Number of Consultations"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful Consultations / Total Consultations"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Decision Tree Consultation Success in Seen Levels"
   set line style 1 lt 6 lw 1
   plot "corrected.uvar.seen" using 1:4 ti "IPLearning (univariate)" with lines, \
        "corrected.mvar.seen" using 1:4 ti "IPLearning (multivariate)" w l ls 1
EOF1
   echo "done :)"
   echo ""
   echo "Plotting application success graph for learned_policy.iplearn.uvarVSmvar.seen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "application_success_uvarVSmvar_seen.eps"
   set xlabel "Number of Applications"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful (Complete) Applications / Total Applications"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Learned Policy Application Success in Seen Levels"
   set line style 1 lt 6 lw 1
   plot "corrected.uvar.seen" using 1:8 ti "IPLearning (univariate)" with lines, \
        "corrected.mvar.seen" using 1:8 ti "IPLearning (multivariate)" w l ls 1
EOF2
   echo "done :)"
   echo "graphs stored in consultation_success_uvarVSmvar_seen.eps and application_success_uvarVSmvar_seen.eps"
  else
   echo "Error: File learned_policy.iplearn.uvar.seen or learned_policy.iplearn.mvar.seen does not exist!"
   exit 1
fi

if [[ -f ./learned_policy.iplearn.uvar.unseen && -f ./learned_policy.iplearn.mvar.unseen ]]
  then
   echo ""
   echo "Plotting consultation success graph for learned_policy.iplearn.uvarVSmvar.unseen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "consultation_success_uvarVSmvar_unseen.eps"
   set xlabel "Number of Consultations"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful Consultations / Total Consultations"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Decision Tree Consultation Success in Unseen Levels"
   set line style 1 lt 6 lw 1
   plot "corrected.uvar.unseen" using 1:4 ti "IPLearning (univariate)" with lines, \
        "corrected.mvar.unseen" using 1:4 ti "IPLearning (multivariate)" w l ls 1
EOF1
   echo "done :)"
   echo ""
   echo "Plotting application success graph for learned_policy.iplearn.uvarVSmvar.unseen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "application_success_uvarVSmvar_unseen.eps"
   set xlabel "Number of Applications"
   set xtics 2500
   set mxtics 10
   set ylabel "Successful (Complete) Applications / Total Applications"
   set yrange [ 0 : 1 ]
   set ytics 0.1
   set mytics 10
   set title "Learned Policy Application Success in Unseen Levels"
   set line style 1 lt 6 lw 1
   plot "corrected.uvar.unseen" using 1:8 ti "IPLearning (univariate)" with lines, \
        "corrected.mvar.unseen" using 1:8 ti "IPLearning (multivariate)" w l ls 1
EOF2
   echo "done :)"
   echo "graphs stored in consultation_success_uvarVSmvar_unseen.eps and application_success_uvarVSmvar_unseen.eps"
  else
   echo "Error: File learned_policy.iplearn.uvar.unseen or learned_policy.iplearn.mvar.unseen does not exist!"
   exit 1
fi

