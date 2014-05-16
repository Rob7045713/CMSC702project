import os
import sys
import shutil

all_words = []
files = {}
indir = os.getcwd() + '/data'
outdir = os.getcwd() + '/data2'

if not os.path.exists(outdir):
    os.makedirs(outdir)

for filename in os.listdir(indir):
    if 'unc' in filename:
        outfile = open(outdir + '/' + filename, 'w')
        with open(indir + '/' + filename, 'r') as f:
            headers = f.readline()
            cis = headers.split('\t')
            outfile.write(cis[0] + '\tbarcode_suffix\t' + '\t'.join(cis[1:]))
            for line in f:
                outfile.write(line[:12] + '\t' + line[12:])
        outfile.close()
                
shutil.copyfile(indir + '/' + 'clinical_patient_coad.txt',
                outdir + '/' + 'clinical_patient_coad.txt')
