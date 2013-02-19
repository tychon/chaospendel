#!/usr/bin/env python

#Import Modules
import os, sys, math, re

def write_pgm(path, rows, cols, maxval, data):
  outf = open(path, "w+")
  outf.write("P2\n"+str(cols)+" "+str(rows)+"\n"+str(maxval)+"\n")
  for row in data:
    for val in row:
      outf.write(str(int(round(val)))+" ");
    outf.write("\n");
  outf.close()

def main():
  # read arguments
  args = sys.argv[1:]
  project_name = None
  steps = 0
  while len(args) > 0:
    arg = args.pop(0)
    if arg == "-n": steps = int(args.pop(0))
    elif not project_name:
      project_name = arg
    else: print "Argument ignored: ", arg
  if not project_name:
    print "Give me some project name!"
    exit(1)
  
  matrix1 = []
  matrix2 = []
  
  for i in range(0, steps):
    print "Step "+str(i)+" / "+str(steps)
    curr_project_name = project_name+str(i)
    
    # Read info file
    print "reading "+curr_project_name+".info ..."
    infof = open(curr_project_name+".info")
    fourier_rows = 0 # number of rows in pgm file
    fourier_window = -1 # size of fourier window
    fourier_scale = -1 # scaling of data in pgm
    for line in infof:
      res = re.match(r"\s*(\S+)\s*=\s*(\S+)", line)
      if res:
        if res.group(1) == "fourier_window": fourier_window  = float(res.group(2))
        elif res.group(1) == "fourier_pgm_scaling": fourier_scale  = float(res.group(2))
    infof.close()
    
    ##### Read first pgm file
    print "reading "+curr_project_name+".pend1.pgm ..."
    pgmf = open(curr_project_name+".pend1.pgm")
    pgmf.readline() # skip first line with type
    # read pgm header
    pgmrow = pgmf.readline().split()
    [fourier_freqn, fourier_rows] = [int(x) for x in pgmrow]
    [pgmmax] = pgmf.readline().split()
    pgmmax = int(pgmmax)
    
    numberstrs = pgmf.readline().split()
    data1 = [float(x) for x in numberstrs]
    matrix1.append(data1)
    
    pgmf.close()
    
    ##### Read second pgm file
    print "reading "+curr_project_name+".pend2.pgm ..."
    pgmf2 = open(curr_project_name+".pend2.pgm")
    pgmf2.readline() # skip first line
    pgmf2.readline() # skip second line
    [pgmmax2] = pgmf2.readline().split() # read out third line
    pgmmax2 = int(pgmmax2)
    
    print "fourier window:", fourier_window
    print "fourier maximum value: "+str(pgmmax/fourier_scale)
    print "number of fourier samples: "+str(fourier_rows)
    print "number of frequ in 1 sample: "+str(fourier_freqn)
    
    numberstrs = pgmf2.readline().split()
    data2 = [float(x) for x in numberstrs]
    matrix2.append(data2)
    
    pgmf2.close()
  
  write_pgm("feigenbaum1.pgm", len(matrix1), fourier_freqn, 256, matrix1)
  write_pgm("feigenbaum2.pgm", len(matrix2), fourier_freqn, 256, matrix2)

#this calls the 'main' function when this script is executed
if __name__ == '__main__': main()

