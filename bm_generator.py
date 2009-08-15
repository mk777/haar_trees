from scipy import random
from math import sqrt
import sys

def make_brownian_path(mean, sigma, numpts):
    xn = sigma * sqrt(numpts)*random.normal()
    result = [ 0.0 for x in range(0,numpts)]
    result[len(result)-1] = xn;
    fill_region(0,len(result)-1,sigma,result)
    return [result[i]+mean*i for i in range (0,numpts)]


def fill_region(l,r,sigma,v):
    if (l == r or l == r-1):
        pass
    else:
        m = int(round((r+l)*0.5))
        a = v[l] + (v[r]-v[l])*(m - l)/float(r - l)
        s = sigma*sqrt((m-l)*(r-m)/float(r-l))
        v[m] = a + s * random.normal()
        fill_region(l,m,sigma,v)
        fill_region(m,r,sigma,v)
        
def make_ar1(c,phi,sigma,numpts):
    result=[0 for x in range(0,numpts)]
    result[0]=c
    for i in range(1,numpts):
        result[i] = c + phi*result[i-1] + sigma*random.normal()
    return result

def make_coint_pair(a1,a2,x0,y0,sigma,c1,c2,phi1,phi2,sigma1,sigma2,numpts):
    bm=make_brownian_path(0.0,sigma,numpts)
    ar1=make_ar1(c1,phi1,sigma1,numpts)
    ar2=make_ar1(c2,phi2,sigma2,numpts)
    return ([x0+a1*x+y for (x,y) in zip(bm,ar1)],
            [y0+a2*x+y for (x,y) in zip(bm,ar2)])

def make_coint_list(bm_coeffs,starting_pts,c_list,phi_list,sigma_list,
                    bm_sigma,numpts):
    bm=make_brownian_path(0.0,bm_sigma,numpts)
    ar_list=map(lambda c,phi,sigma : make_ar1(c,phi,sigma,numpts),c_list,phi_list,
                 sigma_list)
    return map(lambda a,x0,ar1 : map(lambda x,y : x0+a*x+y,bm,ar1),bm_coeffs,
               starting_pts,ar_list);

def read_config_file(filename):
    datafile = open(filename,'r')
    result=[[],[],[],[],[],0.0,0]
    for i in range(0,5):
        l = datafile.readline()
        tokens = l.partition(":")[2].split(',')
        result[i] = map(float,tokens)

    l=datafile.readline()
    sigma=float(l.partition(':')[2])
    result[5] = sigma
    l=datafile.readline()
    numpts=int(l.partition(':')[2])
    result[6] = numpts
    return result

def make_coint_file():
    fname = sys.argv[1]
    config_data=read_config_file(fname)
    print "\n".join(map(lambda x: "\t".join(map(str,x)),zip(*make_coint_list(*config_data))))



if __name__ == "__main__":
    #pass
    make_coint_file()
