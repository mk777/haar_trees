#! /bin/bash

RESFILE=data/coint_results.txt
RESFILE_R=data/coint_results_r.txt
TEMPDATA=data/bm_data.txt

# intialize the result files
echo "# |coint vector">$RESFILE
echo "# |coint vector">$RESFILE_R

for (( i=0; i < ${1:-10}; i++)); do 
  #generate the series
  python bm_generator.pyc bm_config.txt >$TEMPDATA
  
  #calculate the results
  ./haar_coint <$TEMPDATA >> $RESFILE 
  ./haar_coint -r <$TEMPDATA >> $RESFILE_R
done
rm $TEMPDATA

#run the stats calculation on both result files
echo "haar_coint stats starting from the top"
python coint_stat.py $RESFILE
echo "haar_coint stats starting from the bottom"
python coint_stat.py $RESFILE_R
