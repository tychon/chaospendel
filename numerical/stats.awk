
# Read CSV from input and calculate minimum
# and maximum values for columns 5 to 11.
# Output is written as `energy_name = value`

BEGIN {
  getline # read first line to initialize variables
  for (i = 5; i <= 11; i++) {
    min[i] = $i
    max[i] = $i
  }
}

{ for (i = 5; i <= 11; i++) {
    if ($i < min[i]) min[i] = $i
    if ($i > max[i]) max[i] = $i
  }
}

END {
  print "t1min="min[5]
  print "v1min="min[6]
  print "t2min="min[7]
  print "v2min="min[8]
  print "tmin="min[9]
  print "vmin="min[10]
  print "emin="min[11]
  print "t1max="max[5]
  print "v1max="max[6]
  print "t2max="max[7]
  print "v2max="max[8]
  print "tmax="max[9]
  print "vmax="max[10]
  print "emax="max[11]
}

