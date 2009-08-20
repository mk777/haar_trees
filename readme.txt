This is the main directory of the haar_trees project.

bm_config.txt 	-- config info for the time-series generator bm_generator.py

bm_generator.py -- generates a file with columns that are samples of the same 
		scaled browninan motion path plus a different AR(1) process 
		for each of the time series. These are the noisy cointegrated 
		processes that are used to evaluate the efficiency of the Haar
		trees in estimating the cointegration relations

coint_rel_results.sh -- shell script that puts all the moving parts together

coint_stat.py 	-- takes the output of repeated calls to haar_coint and 
		calculates the mean and std div of the components of the
		estimated cointegration relations
coint_stat.pyc

data 		-- dir with temporary data

dataplot.gnu	-- GNU-plot script to make a .png image of a sample time series
		file

haar_coint
haar_coint.cmi
haar_coint.cmo
haar_coint.ml	-- the main OCaml module that estimates the cointegration 
		relations

lib		-- Tree and Haar modules that have all of the required 
		functionality
makefile
results		-- output of calls to coint_rel_results.sh and some other 
		intermediate result files
test		-- misc testing garbage
