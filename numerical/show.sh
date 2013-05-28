#!/bin/bash

make pendulum.x fourier.x ncsvtopgm.x
if [[ $? != 0 ]]; then
  echo "makefile error"
  exit
fi

./pendulum.x > out.csv 2> out.info

echo "l2=0.19" >> out.info

./fourier.x -c 1 < out.csv > out.fourier1.csv
./fourier.x -c 1 < out.csv > out.fourier2.csv

