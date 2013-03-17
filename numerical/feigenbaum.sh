
# Uhh, useful?

source easy_bash.sh

configfile=feigenbaum

STEPS=`doawk steps`
DIRECTORY=`doawk directory`

STEP=`doawk sim_step`
TIME=`doawk time`

if [ "$1" == "simulations" ]; then
  # load angle configs
  ANGLE_OFFSET=$(math `doawk angle_offset`)
  ANGLE_START=$(math `doawk angle_start`)
  ANGLE_END=$(math `doawk angle_end`)
  
  mkdir -p $DIRECTORY # create directory, if it doesn't exist
  ANGLE_STEP=$(math "($ANGLE_START - $ANGLE_END) / $STEPS.0")
  ANGLE=$ANGLE_START
  for X in $(seq 0 $STEPS); do
    echo "simulations: step: "$X" / "$STEPS
    DATAFILE=$DIRECTORY"/out$X.csv"
    INFOFILE=$DIRECTORY"/out$X.info"
    
    # simulation
    MAINARGS="--time $TIME --timestep $STEP --outstep $STEP --phi1 "$(math "$ANGLE+$ANGLE_OFFSET")" --phi2 "$(math "2*$ANGLE+$ANGLE_OFFSET")
    ./pendulum.x $MAINARGS > $DATAFILE 2> $INFOFILE
    
    # Add up angle for next step
    ANGLE=$(math "$ANGLE - $ANGLE_STEP")
  done
fi

if [ "$1" == "fourier" ]; then
  FWINDOW=`doawk fourier_window_max`
  echo -n "fourier: "
  for X in $(seq 0 $STEPS); do
    echo -n "."
    DATAFILE=$DIRECTORY"/out$X.csv"
    INFOFILE=$DIRECTORY"/out$X.info"
    
    ./fourier.x --single -w $FWINDOW -c 0 -i $DATAFILE -o $DIRECTORY"/out$X.pend1.pgm"
    ./fourier.x --single -w $FWINDOW -c 1 -i $DATAFILE -o $DIRECTORY"/out$X.pend2.pgm"
  done
  echo # newline
fi

if [ "$1" == "diagram" ]; then
  python bifurcations.py -i 1 -n $STEPS $DIRECTORY/out
  python bifurcations.py -i 2 -n $STEPS $DIRECTORY/out
fi

