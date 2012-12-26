#!/bin/bash

make

if [ "$1" == "normalisation" ]; then
  if [ "$2" == "replay" ]; then
    echo -e "\nstarting replayer ...\n\n"
    ./replay.x -f -d -t -i values.csv -p data_pendulum -o rawsocket -r 100 &
    sleep 0.1
    echo -e "\nstarting normalisation ...\n\n"
    ./normalisation.x -p data_pendulum -i rawsocket --samplenum 100 > data_normalisation
    echo -e "\nkilling replayer ... \n\n"
    kill $!
  fi
fi

if [ "$1" == "tracking" ]; then
  if [ "$2" == "replay" ]; then
    echo -e "\nstarting replayer ...\n\n"
    ./replay.x -f -d -t -i values.csv -p data_pendulum -o rawsocket -r 1000 > /dev/null&
    sleep 0.001
    echo -e "\nstarting normalisation ...\n\n"
    ./tracker.x -p data_pendulum -n data_normalisation -i rawsocket
    echo -e "\nkilling replayer ... \n\n"
    kill $!
  fi
fi

