#!/bin/bash

source easy_bash.sh

make pendulum.x fourier.x ncsvtopgm.x
if [[ $? != 0 ]]; then
  echo "makefile error"
  exit
fi

./pendulum.x > out.csv 2> out.info

FWINDOW=1000
echo "l2=0.19" >> out.info
echo "fourier1_window=$FWINDOW" >> out.info
echo "fourier1_pgm_scaling=256" >> out.info
echo "fourier2_window=$FWINDOW" >> out.info
echo "fourier2_pgm_scaling=256" >> out.info
FREQN=`math "$FWINDOW / 2 + 1"`
echo $FREQN

./fourier.x -c 1 -w $FWINDOW < out.csv > out.fourier1.csv
./ncsvtopgm.x -c $FREQN -i out.fourier1.csv -o out.pend1.pgm # --maximum --logarithmic --scalerows
./fourier.x -c 1 -w $FWINDOW < out.csv > out.fourier2.csv
./ncsvtopgm.x -c $FREQN -i out.fourier2.csv -o out.pend2.pgm

