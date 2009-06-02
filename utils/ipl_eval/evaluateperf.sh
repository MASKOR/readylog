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
#                the number of solved levels, and the Wumpus World
#                agent's performance measure w.r.t. time and
#                found gold bars.
# last modified: $Date:$
#            by: $Author:$
#
# ###################################################################### #

usage()
{
cat <<EOF
-----------------------------------------------------------------------------
Evaluates the relationship between the number of solved levels, and the
Wumpus World agent's performance measure w.r.t. time and found gold bars.
The input are copies of the the file 'wumpus.log', which is created by the
'run.pl' of the Wumpus World agent.
The copies should be named:
'wumpus.log.dtlearn.seen' for a DT planning agent on (5000) see

Usage: `basename $0` [-levelNum] [-levelsTrained]

options:
        -levelNum: number of levels solved in the log files (default 5000)

        -levelsTrained: number of levels used for training (default 15000).
                        The script assumes that the levels solved in the
                        log files are
                        [levelsTrained + 1, levelsTrained + levelNum]
                        ( default [10001, 15001] )
-----------------------------------------------------------------------------	    
EOF
}

if [[ "$1" = "usage" || "$1" = "-usage" || "$1" = "--usage" ]]
  then
   usage
   exit 1
fi

if [ "$#" -gt 2 ]
  then
   echo Too many arguments!
   usage
   exit 1
fi

if [ "$#" -eq 0 ]
  then
   echo "assumed number of levels in log files: 5000"
   totalRuns=5000
   offset=15000
   echo "offset: $offset levels"
fi

if [ "$#" -eq 1 ]
  then
   echo "assumed number of levels in log files: $1"
   totalRuns=$1
   let offset=15000
   echo "offset: $offset levels"
fi

if [ "$#" -eq 2 ]
  then
   echo "assumed number of levels in log files: $1"
   totalRuns=$1
   let offset=$2
   echo "offset: $offset levels"
fi

### DT planning vs IPL ###

### Seen data ###

### Plot graphs for comparing DT planning and univariate IPL on seen data ###
if [[ -f ./wumpus.log.dtplan.seen && -f ./wumpus.log.iplearn.uvar.seen ]]
  then
   cat wumpus.log.dtplan.seen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.dtplan.seen
   cat wumpus.log.iplearn.uvar.seen | grep '\[' | sed -e 's/\[/ /g' \
                                                      -e 's/\]/ /g' > wumpus.plot.iplearn.uvar.seen
   echo ""
   echo "Plotting time graph for wumpus.log.[dtplan|iplearn.uvar].seen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperftime_uvar_seen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 1000
   set mxtics 10
   set ylabel "Needed Total Time [in Seconds CPU User Time]"
   set yrange [ 0 : 16000 ]
   set ytics 2500
   set mytics 10
   set title "Performance of Wumpus World Agent on Seen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{x=x+\$2; print \$1,x}' wumpus.plot.dtplan.seen" using 1:2 ti "DT planning" with lines, \
        "<awk '{x=x+\$2; print \$1,x}' wumpus.plot.iplearn.uvar.seen" using 1:2 ti "IPLearning (univariate)" w l ls 1
EOF1
   echo "done :)"
   echo ""
   echo "Plotting gold graph for wumpus.log.[dtplan|iplearn.uvar].seen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperfgold_uvar_seen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 2500
   set mxtics 10
   set ylabel "Found Gold Bars [in %]"
   set yrange [ 0 : 40 ]
   set ytics 10
   set mytics 10
   set title "Performance of Wumpus World Agent on Seen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{(\$5 == \"true\") ? x+=1 : x=x; \
                (\$1 > 0) ? z = ((x/\$1)*100) : z = 0; \
                print \$1,z}' wumpus.plot.dtplan.seen" using 1:2 ti "DT planning" with lines, \
        "<awk '{(\$5 == \"true\") ? x+=1 : x=x; \
                (\$1 > 0) ? z = ((x/\$1)*100) : z = 0; \
                print \$1,z}' wumpus.plot.iplearn.uvar.seen" using 1:2 ti "IPLearning (univariate)" w l ls 1
EOF2
   echo "done :)"
   echo "graphs stored in wumpusperftime_uvar_seen.eps and wumpusperfgold_uvar_seen.eps"
  else
   echo "Error: File wumpus.log.dtplan.seen or wumpus.log.iplearn.uvar.seen does not exist!"
fi

### Plot graphs for comparing DT planning and multivariate IPL on seen data ###
if [[ -f ./wumpus.log.dtplan.seen && -f ./wumpus.log.iplearn.mvar.seen ]]
  then
   cat wumpus.log.dtplan.seen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.dtplan.seen
   cat wumpus.log.iplearn.mvar.seen | grep '\[' | sed -e 's/\[/ /g' \
                                                      -e 's/\]/ /g' > wumpus.plot.iplearn.mvar.seen
   echo ""
   echo "Plotting time graph for wumpus.log.[dtplan|iplearn.mvar].seen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperftime_mvar_seen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 1000
   set mxtics 10
   set ylabel "Needed Total Time [in Seconds CPU User Time]"
   set yrange [ 0 : 16000 ]
   set ytics 2500
   set mytics 10
   set title "Performance of Wumpus World Agent on Seen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{x=x+\$2; print \$1,x}' wumpus.plot.dtplan.seen" using 1:2 ti "DT planning" with lines, \
        "<awk '{x=x+\$2; print \$1,x}' wumpus.plot.iplearn.mvar.seen" using 1:2 ti "IPLearning (multivariate)" w l ls 1
EOF1
   echo "done :)"
   echo ""
   echo "Plotting gold graph for wumpus.log.[dtplan|iplearn.mvar].seen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperfgold_mvar_seen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 2500
   set mxtics 10
   set ylabel "Found Gold Bars [in %]"
   set yrange [ 0 : 40 ]
   set ytics 10
   set mytics 10
   set title "Performance of Wumpus World Agent on Seen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{(\$5 == \"true\") ? x+=1 : x=x; \
                (\$1 > 0) ? z = ((x/\$1)*100) : z = 0; \
                print \$1,z}' wumpus.plot.dtplan.seen" using 1:2 ti "DT planning" with lines, \
        "<awk '{(\$5 == \"true\") ? x+=1 : x=x; \
                (\$1 > 0) ? z = ((x/\$1)*100) : z = 0; \
                print \$1,z}' wumpus.plot.iplearn.mvar.seen" using 1:2 ti "IPLearning (multivariate)" w l ls 1
EOF2
   echo "done :)"
   echo "graphs stored in wumpusperftime_mvar_seen.eps and wumpusperfgold_mvar_seen.eps"
  else
   echo "Error: File wumpus.log.dtplan.seen or wumpus.log.iplearn.mvar.seen does not exist!"
fi

### Unseen data ###

### Plot graphs for comparing DT planning and univariate IPL on unseen data ###
if [[ -f ./wumpus.log.dtplan.unseen && -f ./wumpus.log.iplearn.uvar.unseen ]]
  then
   cat wumpus.log.dtplan.unseen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.dtplan.unseen
   cat wumpus.log.iplearn.uvar.unseen | grep '\[' | sed -e 's/\[/ /g' \
                                                      -e 's/\]/ /g' > wumpus.plot.iplearn.uvar.unseen
   echo ""
   echo "Plotting time graph for wumpus.log.[dtplan|iplearn.uvar].unseen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperftime_uvar_unseen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 1000
   set mxtics 10
   set ylabel "Needed Total Time [in Seconds CPU User Time]"
   set yrange [ 0 : 16000 ]
   set ytics 2500
   set mytics 10
   set title "Performance of Wumpus World Agent on Unseen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{y=y+\$2; x=\$1-$offset; print x,y}' wumpus.plot.dtplan.unseen" using 1:2 ti "DT planning" with lines, \
        "<awk '{y=y+\$2; x=\$1-$offset; print x,y}' wumpus.plot.iplearn.uvar.unseen" using 1:2 ti "IPLearning (univariate)" w l ls 1
EOF1
   echo "done :)"
   echo ""
   echo "Plotting gold graph for wumpus.log.[dtplan|iplearn.uvar].unseen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperfgold_uvar_unseen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 2500
   set mxtics 10
   set ylabel "Found Gold Bars [in %]"
   set yrange [ 0 : 40 ]
   set ytics 10
   set mytics 10
   set title "Performance of Wumpus World Agent on Unseen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{x=\$1-$offset; (\$5 == \"true\") ? y+=1 : y=y; \
                (x > 0) ? z = ((y/x)*100) : z = 0; \
                print x,z}' wumpus.plot.dtplan.unseen" using 1:2 ti "DT planning" with lines, \
        "<awk '{x=\$1-$offset; (\$5 == \"true\") ? y+=1 : y=y; \
                (x > 0) ? z = ((y/x)*100) : z = 0; \
                print x,z}' wumpus.plot.iplearn.uvar.unseen" using 1:2 ti "IPLearning (univariate)" w l ls 1
EOF2
   echo "done :)"
   echo "graphs stored in wumpusperftime_uvar_unseen.eps and wumpusperfgold_uvar_unseen.eps"
  else
   echo "Error: File wumpus.log.dtplan.unseen or wumpus.log.iplearn.uvar.unseen does not exist!"
fi

### Plot graphs for comparing DT planning and multivariate IPL on unseen data ###
if [[ -f ./wumpus.log.dtplan.unseen && -f ./wumpus.log.iplearn.mvar.unseen ]]
  then
   cat wumpus.log.dtplan.unseen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.dtplan.unseen
   cat wumpus.log.iplearn.mvar.unseen | grep '\[' | sed -e 's/\[/ /g' \
                                                      -e 's/\]/ /g' > wumpus.plot.iplearn.mvar.unseen
   echo ""
   echo "Plotting time graph for wumpus.log.[dtplan|iplearn.mvar].unseen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperftime_mvar_unseen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 1000
   set mxtics 10
   set ylabel "Needed Total Time [in Seconds CPU User Time]"
   set yrange [ 0 : 16000 ]
   set ytics 2500
   set mytics 10
   set title "Performance of Wumpus World Agent on Unseen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{y=y+\$2; x=\$1-$offset; print x,y}' wumpus.plot.dtplan.unseen" using 1:2 ti "DT planning" with lines, \
        "<awk '{y=y+\$2; x=\$1-$offset; print x,y}' wumpus.plot.iplearn.mvar.unseen" using 1:2 ti "IPLearning (multivariate)" w l ls 1
EOF1
   echo "done :)"
   echo ""
   echo "Plotting gold graph for wumpus.log.[dtplan|iplearn.mvar].unseen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperfgold_mvar_unseen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 2500
   set mxtics 10
   set ylabel "Found Gold Bars [in %]"
   set yrange [ 0 : 40 ]
   set ytics 10
   set mytics 10
   set title "Performance of Wumpus World Agent on Unseen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{x=\$1-$offset; (\$5 == \"true\") ? y+=1 : y=y; \
                (x > 0) ? z = ((y/x)*100) : z = 0; \
                print x,z}' wumpus.plot.dtplan.unseen" using 1:2 ti "DT planning" with lines, \
        "<awk '{x=\$1-$offset; (\$5 == \"true\") ? y+=1 : y=y; \
                (x > 0) ? z = ((y/x)*100) : z = 0; \
                print x,z}' wumpus.plot.iplearn.mvar.unseen" using 1:2 ti "IPLearning (multivariate)" w l ls 1
EOF2
   echo "done :)"
   echo "graphs stored in wumpusperftime_mvar_unseen.eps and wumpusperfgold_mvar_unseen.eps"
  else
   echo "Error: File wumpus.log.dtplan.unseen or wumpus.log.iplearn.mvar.unseen does not exist!"
fi


### Univariate IPL vs. multivariate IPL ###

### Seen data ###

### Plot graphs for comparing univariate IPL and multivariate IPL on seen data ###
if [[ -f ./wumpus.log.iplearn.uvar.seen && -f ./wumpus.log.iplearn.mvar.seen ]]
  then
   cat wumpus.log.iplearn.uvar.seen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.iplearn.uvar.seen
   cat wumpus.log.iplearn.mvar.seen | grep '\[' | sed -e 's/\[/ /g' \
                                                      -e 's/\]/ /g' > wumpus.plot.iplearn.mvar.seen
   echo ""
   echo "Plotting time graph for wumpus.log.[iplearn.uvar|iplearn.mvar].seen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperftime_uvar_vs_mvar_seen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 1000
   set mxtics 10
   set ylabel "Needed Total Time [in Seconds CPU User Time]"
   set yrange [ 0 : 16000 ]
   set ytics 2500
   set mytics 10
   set title "Performance of Wumpus World Agent on Seen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{y=y+\$2; x=\$1; print x,y}' wumpus.plot.iplearn.uvar.seen" using 1:2 ti "IPLearning (univariate)" with lines, \
        "<awk '{y=y+\$2; x=\$1; print x,y}' wumpus.plot.iplearn.mvar.seen" using 1:2 ti "IPLearning (multivariate)" w l ls 1
EOF1
   echo "done :)"
   echo ""
   echo "Plotting gold graph for wumpus.log.[iplearn.uvar|iplearn.mvar].seen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperfgold_uvar_vs_mvar_seen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 2500
   set mxtics 10
   set ylabel "Found Gold Bars [in %]"
   set yrange [ 0 : 40 ]
   set ytics 10
   set mytics 10
   set title "Performance of Wumpus World Agent on Seen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{(\$5 == \"true\") ? x+=1 : x=x; \
                (\$1 > 0) ? z = ((x/\$1)*100) : z = 0; \
                print \$1,z}' wumpus.plot.iplearn.uvar.seen" using 1:2 ti "IPLearning (univariate)" with lines, \
        "<awk '{(\$5 == \"true\") ? x+=1 : x=x; \
                (\$1 > 0) ? z = ((x/\$1)*100) : z = 0; \
                print \$1,z}' wumpus.plot.iplearn.mvar.seen" using 1:2 ti "IPLearning (multivariate)" w l ls 1
EOF2
   echo "done :)"
   echo "graphs stored in wumpusperftime_uvar_vs_mvar_seen.eps and wumpusperfgold_uvar_vs_mvar_seen.eps"
  else
   echo "Error: File wumpus.log.iplearn.uvar.seen or wumpus.log.iplearn.mvar.seen does not exist!"
fi

### Unseen data ###

### Plot graphs for comparing univariate IPL and multivariate IPL on unseen data ###
if [[ -f ./wumpus.log.iplearn.uvar.unseen && -f ./wumpus.log.iplearn.mvar.unseen ]]
  then
   cat wumpus.log.iplearn.uvar.unseen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.iplearn.uvar.unseen
   cat wumpus.log.iplearn.mvar.unseen | grep '\[' | sed -e 's/\[/ /g' \
                                                      -e 's/\]/ /g' > wumpus.plot.iplearn.mvar.unseen
   echo ""
   echo "Plotting time graph for wumpus.log.[iplearn.uvar|iplearn.mvar].unseen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperftime_uvar_vs_mvar_unseen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 1000
   set mxtics 10
   set ylabel "Needed Total Time [in Seconds CPU User Time]"
   set yrange [ 0 : 16000 ]
   set ytics 2500
   set mytics 10
   set title "Performance of Wumpus World Agent on Unseen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{y=y+\$2; x=\$1-$offset; print x,y}' wumpus.plot.iplearn.uvar.unseen" using 1:2 ti "IPLearning (univariate)" with lines, \
        "<awk '{y=y+\$2; x=\$1-$offset; print x,y}' wumpus.plot.iplearn.mvar.unseen" using 1:2 ti "IPLearning (multivariate)" w l ls 1
EOF1
   echo "done :)"
   echo ""
   echo "Plotting gold graph for wumpus.log.[iplearn.uvar|iplearn.mvar].unseen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperfgold_uvar_vs_mvar_unseen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 2500
   set mxtics 10
   set ylabel "Found Gold Bars [in %]"
   set yrange [ 0 : 40 ]
   set ytics 10
   set mytics 10
   set title "Performance of Wumpus World Agent on Seen Levels"
   set line style 1 lt 6 lw 1
   plot "<awk '{x=\$1-$offset; (\$5 == \"true\") ? y+=1 : y=y; \
                (x > 0) ? z = ((y/x)*100) : z = 0; \
                print x,z}' wumpus.plot.iplearn.uvar.unseen" using 1:2 ti "IPLearning (univariate)" with lines, \
        "<awk '{x=\$1-$offset; (\$5 == \"true\") ? y+=1 : y=y; \
                (x > 0) ? z = ((y/x)*100) : z = 0; \
                print x,z}' wumpus.plot.iplearn.mvar.unseen" using 1:2 ti "IPLearning (multivariate)" w l ls 1
EOF2
   echo "done :)"
   echo "graphs stored in wumpusperftime_uvar_vs_mvar_unseen.eps and wumpusperfgold_uvar_vs_mvar_unseen.eps"
  else
   echo "Error: File wumpus.log.iplearn.uvar.unseen or wumpus.log.iplearn.mvar.unseen does not exist!"
fi

### DT planning vs univariate IPL vs multivariate IPL ###

### Seen data ###
if [[ -f ./wumpus.log.dtplan.seen && -f ./wumpus.log.iplearn.uvar.seen && -f ./wumpus.log.iplearn.mvar.seen ]]
  then
   cat wumpus.log.dtplan.seen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.dtplan.seen
   cat wumpus.log.iplearn.uvar.seen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.iplearn.uvar.seen
   cat wumpus.log.iplearn.mvar.seen | grep '\[' | sed -e 's/\[/ /g' \
                                                      -e 's/\]/ /g' > wumpus.plot.iplearn.mvar.seen
   echo ""
   echo "Plotting time graph for wumpus.log.[dtplan|iplearn.uvar|iplearn.mvar].seen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperftime_dtplan_vs_uvar_vs_mvar_seen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 1000
   set mxtics 10
   set ylabel "Needed Total Time [in Seconds CPU User Time]"
   set yrange [ 0 : 16000 ]
   set ytics 2500
   set mytics 10
   set title "Performance of Wumpus World Agent on Seen Levels"
   set line style 1 lt 6 lw 1
   set line style 2 lt 0 lw 1
   plot "<awk '{y=y+\$2; x=\$1; print x,y}' wumpus.plot.dtplan.seen" using 1:2 ti "DT planning" with lines, \
        "<awk '{y=y+\$2; x=\$1; print x,y}' wumpus.plot.iplearn.uvar.seen" using 1:2 ti "IPLearning (univariate)" w l ls 1, \
        "<awk '{y=y+\$2; x=\$1; print x,y}' wumpus.plot.iplearn.mvar.seen" using 1:2 ti "IPLearning (multivariate)" w l ls 2
EOF1
   echo "done :)"
   echo ""
   echo "Plotting gold graph for wumpus.log.[dtplan|iplearn.uvar|iplearn.mvar].seen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperfgold_dtplan_vs_uvar_vs_mvar_seen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 2500
   set mxtics 10
   set ylabel "Found Gold Bars [in %]"
   set yrange [ 0 : 40 ]
   set ytics 10
   set mytics 10
   set title "Performance of Wumpus World Agent on Seen Levels"
   set line style 1 lt 6 lw 1
   set line style 2 lt 0 lw 1
   plot "<awk '{(\$5 == \"true\") ? x+=1 : x=x; \
                (\$1 > 0) ? z = ((x/\$1)*100) : z = 0; \
                print \$1,z}' wumpus.plot.dtplan.seen" using 1:2 ti "DT planning" with lines, \
        "<awk '{(\$5 == \"true\") ? x+=1 : x=x; \
                (\$1 > 0) ? z = ((x/\$1)*100) : z = 0; \
                print \$1,z}' wumpus.plot.iplearn.uvar.seen" using 1:2 ti "IPLearning (univariate)" w l ls 1, \
        "<awk '{(\$5 == \"true\") ? x+=1 : x=x; \
                (\$1 > 0) ? z = ((x/\$1)*100) : z = 0; \
                print \$1,z}' wumpus.plot.iplearn.mvar.seen" using 1:2 ti "IPLearning (multivariate)" w l ls 2
EOF2
   echo "done :)"
   echo "graphs stored in wumpusperftime_dtplan_vs_uvar_vs_mvar_seen.eps and wumpusperfgold_dtplan_vs_uvar_vs_mvar_seen.eps"
  else
   echo "Error: File wumpus.log.dtplan.seen wumpus.log.iplearn.uvar.seen or wumpus.log.iplearn.mvar.seen does not exist!"
fi

### Unseen data ###
if [[ -f ./wumpus.log.dtplan.unseen && -f ./wumpus.log.iplearn.uvar.unseen && -f ./wumpus.log.iplearn.mvar.unseen ]]
  then
   cat wumpus.log.dtplan.unseen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.dtplan.unseen
   cat wumpus.log.iplearn.uvar.unseen | grep '\[' | sed -e 's/\[/ /g' \
                                                -e 's/\]/ /g' > wumpus.plot.iplearn.uvar.unseen
   cat wumpus.log.iplearn.mvar.unseen | grep '\[' | sed -e 's/\[/ /g' \
                                                      -e 's/\]/ /g' > wumpus.plot.iplearn.mvar.unseen
   echo ""
   echo "Plotting time graph for wumpus.log.[dtplan|iplearn.uvar|iplearn.mvar].unseen..."
   gnuplot << EOF1
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperftime_dtplan_vs_uvar_vs_mvar_unseen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 1000
   set mxtics 10
   set ylabel "Needed Total Time [in Seconds CPU User Time]"
   set yrange [ 0 : 16000 ]
   set ytics 2500
   set mytics 10
   set title "Performance of Wumpus World Agent on Unseen Levels"
   set line style 1 lt 6 lw 1
   set line style 2 lt 0 lw 1
   plot "<awk '{y=y+\$2; x=\$1-$offset; print x,y}' wumpus.plot.dtplan.unseen" using 1:2 ti "DT planning" with lines, \
        "<awk '{y=y+\$2; x=\$1-$offset; print x,y}' wumpus.plot.iplearn.uvar.unseen" using 1:2 ti "IPLearning (univariate)" w l ls 1, \
        "<awk '{y=y+\$2; x=\$1-$offset; print x,y}' wumpus.plot.iplearn.mvar.unseen" using 1:2 ti "IPLearning (multivariate)" w l ls 2
EOF1
   echo "done :)"
   echo ""
   echo "Plotting gold graph for wumpus.log.[dtplan|iplearn.uvar|iplearn.mvar].unseen..."
   gnuplot << EOF2
   set terminal postscript eps enhanced
   set key right bottom
   set output "wumpusperfgold_dtplan_vs_uvar_vs_mvar_unseen.eps"
   set xlabel "Solved Levels"
   set xrange [ 0 : $totalRuns ]
   set xtics 2500
   set mxtics 10
   set ylabel "Found Gold Bars [in %]"
   set yrange [ 0 : 40 ]
   set ytics 10
   set mytics 10
   set title "Performance of Wumpus World Agent on Unseen Levels"
   set line style 1 lt 6 lw 1
   set line style 2 lt 0 lw 1
   plot "<awk '{x=\$1-$offset; (\$5 == \"true\") ? y+=1 : y=y; \
                (x > 0) ? z = ((y/x)*100) : z = 0; \
                print x,z}' wumpus.plot.dtplan.unseen" using 1:2 ti "DT planning" with lines, \
        "<awk '{x=\$1-$offset; (\$5 == \"true\") ? y+=1 : y=y; \
                (x > 0) ? z = ((y/x)*100) : z = 0; \
                print x,z}' wumpus.plot.iplearn.uvar.unseen" using 1:2 ti "IPLearning (univariate)" w l ls 1, \
        "<awk '{x=\$1-$offset; (\$5 == \"true\") ? y+=1 : y=y; \
                (x > 0) ? z = ((y/x)*100) : z = 0; \
                print x,z}' wumpus.plot.iplearn.mvar.unseen" using 1:2 ti "IPLearning (multivariate)" w l ls 2
EOF2
   echo "done :)"
   echo "graphs stored in wumpusperftime_dtplan_vs_uvar_vs_mvar_unseen.eps and wumpusperfgold_dtplan_vs_uvar_vs_mvar_unseen.eps"
  else
   echo "Error: File wumpus.log.dtplan.unseen wumpus.log.iplearn.uvar.unseen or wumpus.log.iplearn.mvar.unseen does not exist!"
fi
