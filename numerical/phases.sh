
make
if [[ $? != 0 ]]; then
  echo "makefile error"
  exit
fi

source easy_bash.sh

configfile=phases

STEPS=`doawk steps`
DIRECTORY=`doawk directory`

OUTSTEP=`doawk simout_step`
TIME=`doawk time`


if [[ "$1" == "simulations" ]]; then
  # load angle configs
  ANGLE_OFFSET=$(math `doawk angle_offset`)
  ANGLE_START=$(math `doawk angle_start`)
  ANGLE_END=$(math `doawk angle_end`)
  
  mkdir -p $DIRECTORY # create directory, if it doesn't exist
  
  ANGLE_STEP=$(math "($ANGLE_START - $ANGLE_END) / $STEPS.0")
  ANGLE=$ANGLE_START
  for X in $(seq 0 $STEPS); do
    echo "step: "$X" / "$STEPS
    DATAFILE=$DIRECTORY"/out$X.csv"
    
    # simulation
    MAINARGS="--time $TIME --outstep $OUTSTEP --phi1 "$(math "$ANGLE+$ANGLE_OFFSET")" --phi2 "$(math "2*$ANGLE+$ANGLE_OFFSET")
    ./pendulum.x $MAINARGS > $DATAFILE 2> /dev/null
    
    # Add up angle for next step
    ANGLE=$(math "$ANGLE - $ANGLE_STEP")
  done
fi

if [[ "$1" == "plots" ]]; then
  echo -n "plots: "
  for X in $(seq 0 $STEPS); do
    echo -n "."
    DATAFILE=$DIRECTORY"/out$X.csv"
    
    # octave
    #octave --eval "A=csvread(\"$DIRECTORY/out$X.csv\"); plot3(A(:,1),A(:,2),A(:,4)); axis([-pi, pi, -pi, pi, -pi, pi]); print -dpng \"$DIRECTORY/out$X.png\";" > /dev/null
    octave --eval "A=csvread(\"$DIRECTORY/out$X.csv\"); plot3(A(30000:48000,1),A(30000:48000,2),A(30000:48000,4)); axis(\"square\"); print -dpng \"$DIRECTORY/out$X.png\";" > /dev/null
    if [[ $? != 0 ]]; then exit; fi
  done
fi

