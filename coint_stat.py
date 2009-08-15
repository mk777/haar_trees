from __future__ import with_statement
from math import sqrt
import sys

def read_data(filename):
    """reads the file of results printed by haar_coint"""

    result={}
    with open(filename,'r') as file:
        for l in file:
            (n,s,vec)=l.partition('[');
            if s: 
                result.setdefault(int(n),[]).append(
                    [float(num) for num in vec.split(' ')[:-1]])
    return result

def result_avg(res_data):
    """averages the results for each key"""

    return dict((k,map(lambda x: sum(x)/float(len(x)),zip(*v))) for k,v in 
                res_data.iteritems())
  
def result_std_div(res_data,res_avg):
    """returns std diviation for each component for each key"""
    aux = dict((k,zip(res_avg[k],zip(*v))) for k,v in res_data.iteritems()) 
    return dict((k,map(lambda (a,lst) : 
                   sqrt(sum(map(lambda x: (x-a)*(x-a),lst))/(len(lst)-1.0)),v))
                for k,v in aux.iteritems())

def print_stats(avg,stdev):
    """pretty prints the stats"""
    for k in avg.keys():
        print str(k)+": avg\t["+ " ".join(map(lambda x: "%.4f" % x,avg[k]))+"]"
        print "  stdev\t[" +" ".join(map(lambda x: "%.4f" % x,stdev[k]))+"]"

if __name__ == "__main__":
    filename=sys.argv[1]
    print filename
    d=read_data(filename)
    avg=result_avg(d)
    stdev=result_std_div(d,avg)

    print_stats(avg,stdev)

