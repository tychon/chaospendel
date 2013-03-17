
PI=3.141592653589793238462643383279502884197169

# Retrieve one right hand value of one line in the data_pendulum file
# where keys and values are separated by equals signs.
function doawk {
  echo $(awk -F "=" '/'$1'/ {print $2}' $configfile)
}

# Execute math with bc and add leading zeroes if necessary: .234 to 0.234
function math {
  echo $(echo "pi=$PI;"$1 | bc -l | sed 's/^\./0./')
}

