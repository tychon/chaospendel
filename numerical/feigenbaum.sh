
# Uhh, useful?
PI=3.141592653589793238462643383279502884197169

STEPS=$(awk -F "=" '/steps/ {print $2}' feigenbaum)

DIRECTORY=$(awk -F "=" '/directory/ {print $2}' feigenbaum)

if [ "$1" == "simulations" ]; then
  # load angle configs
  ANGLE_OFFSET=$(awk -F "=" '/angle_offset/ {print $2}' feigenbaum)
  ANGLE_OFFSET=$(echo "pi=$PI;"$ANGLE_OFFSET | bc -l)
  ANGLE_START=$(awk -F "=" '/angle_start/ {print $2}' feigenbaum)
  ANGLE_START=$(echo "pi=$PI;"$ANGLE_START | bc -l)
  ANGLE_END=$(awk -F "=" '/angle_end/ {print $2}' feigenbaum)
  ANGLE_END=$(echo "pi=$PI;"$ANGLE_END | bc -l)
  
  mkdir -p $DIRECTORY # create directory, if it doesn't exist
  ANGLE_STEP=$(echo "($ANGLE_START - $ANGLE_END) / $STEPS.0" | bc -l)
  ANGLE=$ANGLE_START
  for X in $(seq 0 $STEPS); do
    echo "simulations: step: "$X" / "$STEPS
    DATAFILE=$DIRECTORY"/out$X.csv"
    INFOFILE=$DIRECTORY"/out$X.info"
    
    # simulation
    MAINARGS="1 "$(echo "$ANGLE+$ANGLE_OFFSET" | bc -l)" 2 "$(echo "2*$ANGLE+$ANGLE_OFFSET" | bc -l)
    ./main.x $MAINARGS > $DATAFILE 2> $INFOFILE
    
    # energy stats
    awk -F "," -f stats.awk $DATAFILE >> $INFOFILE
    
    # Add up angle for next step
    ANGLE=$(echo "$ANGLE - $ANGLE_STEP" | bc -l)
  done
fi

CRASHTIMEFILE=$(awk -F "=" '/crash_times/ {print $2}' feigenbaum)

if [ "$1" == "crashes" ]; then
  #CRASHRATIO=$(awk -F "=" '/crash_energy_ratio/ {print $2}' feigenbaum)
  echo "" > feigenbaum_crashtimes.csv # Clear old data
  echo -n "crashes: "
  for X in $(seq 0 $STEPS); do
    echo -n "." # Some feedback to the user
    DATAFILE=$DIRECTORY"/out$X.csv"
    CRASHTIME=$(awk -F "," -f print_crash_time.awk $DATAFILE)
    echo $X","$CRASHTIME >> feigenbaum_crashtimes.csv
  done
  echo # newline
fi

if [ "$1" == "fourier" ]; then
  FWINDOW=$(awk -F "=" '/fourier_window_max/ {print $2}' feigenbaum)
  echo -n "fourier: "
  for X in $(seq 0 $STEPS); do
    echo -n "."
    DATAFILE=$DIRECTORY"/out$X.csv"
    INFOFILE=$DIRECTORY"/out$X.info"
    
    # TODO load crash time and shorten window if necessary
    
    ./fourier.x --inputfile $DATAFILE --samples `cat $DATAFILE | wc -l` --window $FWINDOW --column 0 --projectfile $INFOFILE --outputfile $DIRECTORY"/out$X.pend1.pgm" --keyprefix fourier1 > /dev/null
    ./fourier.x --inputfile $DATAFILE --samples `cat $DATAFILE | wc -l` --window $FWINDOW --column 1 --projectfile $INFOFILE --outputfile $DIRECTORY"/out$X.pend2.pgm" --keyprefix fourier2 > /dev/null
  done
  echo # newline
fi

if [ "$1" == "diagram" ]; then
  python bifurcations.py -i 1 -n $STEPS $DIRECTORY/out
  python bifurcations.py -i 2 -n $STEPS $DIRECTORY/out
fi

