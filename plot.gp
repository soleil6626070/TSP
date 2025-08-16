set terminal png size 800,800
set output 'tsp_path.png'
set title 'Best TSP Path'
set xlabel 'X Coordinate'
set ylabel 'Y Coordinate'
set grid
set size ratio 1
plot 'CitiesPlot.txt' using 1:2 with linespoints linewidth 2 pointtype 7 pointsize 1.5 title 'TSP Path'