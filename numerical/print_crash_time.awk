
# Read CSV from input and calculate for every sample the ratio between the
# current total energy and the total energy in the beginning.
# If the absolute ratio is > 2 or < 0.5

BEGIN {
  getline # read first line to initialize variables
  start = $11
  sample = 1
}

{ sample = sample + 1
  diff = $11/start
  absdiff = diff >= 0.0 ? diff : diff * -1
  if (absdiff < 0.9 || absdiff > 1.1) {
    printf "%d,%f", sample, diff
    exit
  }
}

