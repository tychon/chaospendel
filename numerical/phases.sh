
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

# load angle configs
PHI1_START=$(math `doawk phi1_start`)
PHI1_END=$(math `doawk phi1_end`)
PHI2_START=$(math `doawk phi2_start`)
PHI2_END=$(math `doawk phi2_end`)

mkdir -p $DIRECTORY # create directory, if it doesn't exist

PHI1_STEP=$(math "($PHI1_START - $PHI1_END) / $STEPS.0")
PHI2_STEP=$(math "($PHI2_START - $PHI2_END) / $STEPS.0")
PHI1=$PHI1_START
PHI2=$PHI2_START
echo -e "step\tphi1\tphi2"
for X in $(seq 0 $STEPS); do
  echo -e "$X/$STEPS\t$PHI1\t$PHI2"
  DATAFILE=$DIRECTORY"/out$X.csv"
  IMAGEFILE=$DIRECTORY"/out$X.png"
  
  # simulation
  MAINARGS="--time $TIME --outstep $OUTSTEP --phi1 $PHI1 --phi2 $PHI2"
  ./pendulum.x $MAINARGS > $DATAFILE 2> /dev/null
  
  octave --eval "A=csvread(\"$DATAFILE\");
    plot3(A(:,1),A(:,2),A(:,4));
    title(\"phi1_0=$PHI1, phi2_0=$PHI2\");
    xlabel(\"phi1\");
    ylabel(\"phi2\");
    zlabel(\"p2\");
    axis(\"square\");
    print -dpng \"$IMAGEFILE\";" > /dev/null
  
  # Add up angles for next step
  PHI1=$(math "$PHI1 - $PHI1_STEP")
  PHI2=$(math "$PHI2 - $PHI2_STEP")
done

