import os
import sys

all_words = []
files = {}
dir = os.getcwd() + '/data'
outdir = os.getcwd() + '/data2'
for filename in os.listdir(dir):
    if 'unc' in filename:
        outfile = open(outdir + '/' + filename, 'w')
        with open(dir + '/' + filename, 'r') as f:
            headers = f.readline()
            cis = headers.split('\t')
            outfile.write(cis[0] + '\tbarcode_suffix\t' + '\t'.join(cis[1:]))
            for line in f:
                outfile.write(line[:12] + '\t' + line[12:])
        outfile.close()
                

