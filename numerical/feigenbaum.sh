
make
if [[ $? != 0 ]]; then
  echo "makefile error"
  exit
fi

source easy_bash.sh

configfile=feigenbaum

STEPS=`doawk steps`
DIRECTORY=`doawk directory`

OUTSTEP=`doawk simout_step`
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
    MAINARGS="--time $TIME --outstep $OUTSTEP --phi1 "$(math "$ANGLE+$ANGLE_OFFSET")" --phi2 "$(math "2*$ANGLE+$ANGLE_OFFSET")
    ./pendulum.x $MAINARGS > $DATAFILE 2> $INFOFILE
    
    # Add up angle for next step
    ANGLE=$(math "$ANGLE - $ANGLE_STEP")
  done
fi

FWINDOW=`math "$TIME / $OUTSTEP"`
FREQN=`math "$FWINDOW / 2 + 1"`

# Requires run with "simulations"
if [ "$1" == "fourier" ]; then
  echo -n "fourier: "
  for X in $(seq 0 $STEPS); do
    echo -n "."
    DATAFILE=$DIRECTORY"/out$X.csv"
    INFOFILE=$DIRECTORY"/out$X.info"
    
    ./fourier.x --single -w $FWINDOW -c 0 -i $DATAFILE -o $DIRECTORY"/out$X.fourier1.csv"
    ./fourier.x --single -w $FWINDOW -c 1 -i $DATAFILE -o $DIRECTORY"/out$X.fourier2.csv"
  done
  echo # newline
fi

# Requires run with "fourier"
if [ "$1" == "pgms" ]; then
  echo -n "pgms: "
  for X in $(seq 0 $STEPS); do
    echo -n "."
    ./ncsvtopgm.x -c $FREQN -i $DIRECTORY"/out$X.fourier1.csv" -o $DIRECTORY"/out$X.fourier1.pgm" 2> /dev/null
    ./ncsvtopgm.x -c $FREQN -i $DIRECTORY"/out$X.fourier2.csv" -o $DIRECTORY"/out$X.fourier2.pgm" 2> /dev/null
  done
  echo # newline
fi

# Requires run with "fourier"
if [ "$1" == "diagram" ]; then
  cat $DIRECTORY/*.fourier1.csv > $DIRECTORY/fourier1.csv
  cat $DIRECTORY/*.fourier2.csv > $DIRECTORY/fourier2.csv
  ./ncsvtopgm.x --scalerows --logarithmic --invert --maximum -c $FREQN -i $DIRECTORY/fourier1.csv -o $DIRECTORY/fourier1.pgm 2> /dev/null
  ./ncsvtopgm.x --scalerows --logarithmic --invert --maximum -c $FREQN -i $DIRECTORY/fourier2.csv -o $DIRECTORY/fourier2.pgm 2> /dev/null
fi

if [ "$1" == "all" ]; then
  ./feigenbaum.sh simulations
  ./feigenbaum.sh fourier
  ./feigenbaum.sh diagram
fi
