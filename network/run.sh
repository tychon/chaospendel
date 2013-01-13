#!/bin/bash

make

if [ "$1" == "rangetest" ]; then
  # start arduino reader
  xterm -e ./reader.x &
  sleep 1
  ./rangetester.x > data_rangetest.csv
fi

if [ "$1" == "normalisation" ]; then
  if [ "$2" == "replay" ]; then
    ./replay.x -f -d -r 1000 &
    sleep 0.0001
    ./normalisation.x --samplenum 1000 > data_normalisation
    kill $!
  else
    xterm -e ./reader.x &
    sleep 1
    ./normalisation.x --samplenum 10000 > data_normalisation
    kill $!
  fi
fi

if [ "$1" == "tracking" ]; then
  if [ "$2" == "fastreplay" ]; then
    ./replay.x -f -d -r 100000 > /dev/null &
    sleep 0.00000001
    ./tracker.x --showoverflows --printtempdata > data_tracking_lasttemporary.csv
  elif [ "$2" == "replay" ]; then
    ./replay.x -f -d -i data_values_lastreplay.csv -p data_pendulum -o socket_replay -r 550 > /dev/null &
    sleep 0.01
    ./tracker.x --showx11gui --showoverflows --printtempdata > data_tracking_lasttemporary.csv
  else
    xterm -e ./reader.x &
    sleep 1
    ./tracker.x --showx11gui --maxframerate 50 --showoverflows --printtempdata > data_tracking_lasttemporary.csv
  fi
fi

if [ "$1" == "collectmarkov" ]; then
  if [ "$2" == "replay" ]; then
    xterm -e ./replay.x -f -d -r 550 &
    sleep 0.5
    ./tracker.x --showoverflows &
    sleep 0.5
    if [ -e "data_markovchain" ]; then
      ./markov_prediction.x -mo data_markov
    else
      ./markov_prediction.x -mi NULL data_markov
    fi
  else
    xterm -e ./reader.x &
    sleep 0.5
    xterm -e ./tracker.x --showoverflows &
    sleep 0.5
    if [ -e "data_markovchain" ]; then
      ./markov_prediction.x -mo data_markovchain
    else
      ./markov_prediction.x -mi NULL -mo data_markovchain
    fi
  fi
fi

if [ "$1" == "prediction" ]; then
  if [ "$2" == "replay" ]; then
    xterm -e ./replay.x -f -d -r 550 &
    sleep 0.5
    xterm -e ./tracker.x --showoverflows &
    sleep 0.5
    ./markov_prediction.x
  else
    xterm -e ./reader.x &
    sleep 0.5
    xterm -e ./tracker.x --showoverflows &
    sleep 0.5
    ./markov_prediction.x
  fi
fi


if [ "$1" == "minen" ]; then
  xterm -e ./reader.x &
  sleep 0.5
  xterm -e ./tracker.x --showoverflows &
  sleep 0.5
  ./markov_prediction.x --manip socket_arduino --minen
fi

