#! /bin/bash

RESFILE=data/coint_results.txt

echo "# |coint vector">$RESFILE
for (( i=0; i < ${1:-1}; i++)); do
  python bm_generator.pyc bm_config.txt | ./haar_coint >> $RESFILE 
done

python coint_stat.py $RESFILE
