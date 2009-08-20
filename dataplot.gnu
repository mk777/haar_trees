#!/usr/bin/gnuplot
# this script is used to gnuplot the data in a file

set term png
set output "data/data_graph.png"

plot "data/sample.txt" using 0:1 title "Data1" with lines,\
"" using 0:2 title "Data2" with lines,\
"" using 0:3 title "Data3" with lines,\
"" using 0:4 title "Data4" with lines

