# Forest Plot Generator
A script to generate forest plots automatically.

The functions are as described below:

## forestPlotMatrix 
Take a erighted cluster matrix as an input and generates a new matrix 
with an addition of new rows for multi-cluster variants and new column specifying the cluster assignment.

## generatePlot 
Takes a matrix with the below columns as input
- beta
- se
- locus
- cluster

Returns a forest plot. (Please change plot title to an appropriate string)

