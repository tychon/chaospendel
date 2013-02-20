#!/usr/bin/env python

#Import Modules
import os, sys, math, re

def write_pgm(path, rows, cols, maxval, data, invert=False):
  outf = open(path, "w+")
  outf.write("P2\n"+str(cols)+" "+str(rows)+"\n"+str(maxval)+"\n")
  for row in data:
    for val in row:
      if invert: outf.write(str(int(round(maxval-val)))+" ")
      else: outf.write(str(int(round(val)))+" ")
    outf.write("\n")
  outf.close()

def main():
  # read arguments
  args = sys.argv[1:]
  project_name = None
  steps = 0
  index = 1
  while len(args) > 0:
    arg = args.pop(0)
    if arg == "-n": steps = int(args.pop(0))
    elif arg == "-i": index = int(args.pop(0))
    elif not project_name:
      project_name = arg
    else: print "Argument ignored: ", arg
  if not project_name:
    print "Give me some project name!"
    exit(1)
  
  matrix = []
  
  for i in range(0, steps+1):
    print "bifurcations "+str(index)+": Step "+str(i)+" / "+str(steps)
    curr_project_name = project_name+str(i)
    
    # Read info file
    #print "reading "+curr_project_name+".info ..."
    infof = open(curr_project_name+".info")
    fourier_rows = 0 # number of rows in pgm file
    fourier_window = -1 # size of fourier window
    fourier_scale = -1 # scaling of data in pgm
    for line in infof:
      res = re.match(r"\s*(\S+)\s*=\s*(\S+)", line)
      if res:
        if res.group(1) == "fourier"+str(index)+"_window": fourier_window  = float(res.group(2))
        elif res.group(1) == "fourier"+str(index)+"_pgm_scaling": fourier_scale  = float(res.group(2))
    infof.close()
    
    ##### Read first pgm file
    #print "reading "+curr_project_name+".pend1.pgm ..."
    pgmf = open(curr_project_name+".pend"+str(index)+".pgm")
    pgmf.readline() # skip first line with type
    # read pgm header
    pgmrow = pgmf.readline().split()
    [fourier_freqn, fourier_rows] = [int(x) for x in pgmrow]
    [pgmmax] = pgmf.readline().split()
    pgmmax = int(pgmmax)
    
    numberstrs = pgmf.readline().split()
    data = [float(x) for x in numberstrs]
    matrix.append(data)
    
    pgmf.close()
    
    #print "fourier window:", fourier_window
    #print "fourier maximum value: "+str(pgmmax/fourier_scale)
    #print "number of fourier samples: "+str(fourier_rows)
    #print "number of frequ in 1 sample: "+str(fourier_freqn)
  
  write_pgm("feigenbaum"+str(index)+".pgm", len(matrix), fourier_freqn, 256, matrix, True)

#this calls the 'main' function when this script is executed
if __name__ == '__main__': main()

