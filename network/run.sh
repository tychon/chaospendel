#!/bin/bash

make

if [ "$1" == "normalisation" ]; then
  if [ "$2" == "replay" ]; then
    # start replayer in background
    echo -e "\nstarting replayer ...\n\n"
    ./replay.x -f -d -nt -i data_values_normalisation.csv -p data_pendulum -o rawsocket -r 100 &
    
    sleep 0.1
    echo -e "\nstarting normalisation ...\n\n"
    ./normalisation.x -p data_pendulum -i rawsocket --samplenum 1000 > data_normalisation
    
    echo -e "\nkilling replayer ... \n\n"
    kill $!
  fi
fi

if [ "$1" == "tracking" ]; then
  if [ "$2" == "replay" ]; then
    # start replayer in background
    echo -e "\nstarting replayer ...\n\n"
    ./replay.x -f -d -nt -i data_values.csv -p data_pendulum -o rawsocket -r 100000 > /dev/null &
    
    sleep 0.000000001
    echo -e "\nstarting tracker ...\n\n"
    ./tracker.x -p data_pendulum -n data_normalisation -i rawsocket -o anglessocket --printtemp > data_tracking_temporary.csv
    
    # killing of replayer not necessary, because the tracker runs
    # til end of data from replayer
  fi
fi

