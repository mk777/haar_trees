import sys

def make_linear_list(coeff,numpts):
    rng = range(0,numpts)
    return [[x * t for t in rng] for x in coeff]
    

def make_linear_file():
    coeff = map (float,sys.argv[1:-1])
    numpts = int(sys.argv[-1])
    print "\n".join(map(lambda x: "\t".join(map(str,x)),zip(*make_linear_list(coeff,numpts))))

if __name__ == "__main__":
    #pass
    make_linear_file()
