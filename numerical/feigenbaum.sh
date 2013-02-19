
# Uhh, useful?
PI=3.141592653589793238462643383279502884197169

# Change settings here:
ANGLE_OFFSET=$PI
STEPS=1000
START_ANGLE=0.0
MAX_ANGLE=$(echo "$PI/3" | bc -l)

DIRECTORY=feigenbaum-data
FWINDOW=512
CRASHTIMEFILE=feigenbaum_crashtimes.csv

if [ "$1" == "simulations" ]; then
  mkdir -p $DIRECTORY
  ANGLE_STEP=$(echo "($START_ANGLE-$MAX_ANGLE) / $STEPS.0" | bc -l)
  ANGLE=$START_ANGLE
  for X in $(seq 0 $STEPS); do
    echo Step: $X / $STEPS
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

if [ "$1" == "crashes" ]; then
  echo "" > feigenbaum_crashtimes.csv
  for X in $(seq 0 $STEPS); do
    echo -n "."
    DATAFILE=$DIRECTORY"/out$X.csv"
    CRASHTIME=$(awk -F "," -f print_crash_time.awk $DATAFILE)
    echo $X"  "$CRASHTIME >> feigenbaum_crashtimes.csv
  done
  echo
fi

if [ "$1" == "fourier" ]; then
  for X in $(seq 0 $STEPS); do
    DATAFILE=$DIRECTORY"/out$X.csv"
    INFOFILE=$DIRECTORY"/out$X.info"
    
    ./fourier.x --inputfile $DATAFILE --samples `cat $DATAFILE | wc -l` --window $FWINDOW --column 0 --projectfile $INFOFILE --outputfile $DIRECTORY"/out$X.pend1.pgm" > /dev/null
    ./fourier.x --inputfile $DATAFILE --samples `cat $DATAFILE | wc -l` --window $FWINDOW --column 1 --projectfile $INFOFILE --outputfile $DIRECTORY"/out$X.pend2.pgm" > /dev/null
  done
fi

if [ "$1" == "diagram" ]; then
  python bifurcations.py -n $STEPS $DIRECTORY/out
fi

