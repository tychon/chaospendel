#!/bin/bash

make

if [ "$1" == "rangetest" ]; then
  # start arduino reader
  xterm -e ./reader.x -p data_pendulum -o socket_arduino &
  sleep 1
  ./rangetester.x -p data_pendulum -n data_normalisation -i socket_arduino > data_rangetest.csv
fi

if [ "$1" == "normalisation" ]; then
  if [ "$2" == "replay" ]; then
    # start replayer in background
    echo -e "\nstarting replayer ...\n\n"
    ./replay.x -f -d -nt -i data_values_normalisation.csv -p data_pendulum -o socket_replay -r 100 &
    
    sleep 0.1
    echo -e "\nstarting normalisation ...\n\n"
    ./normalisation.x -p data_pendulum -i socket_replay --samplenum 1000 > data_normalisation
    
    echo -e "\nkilling replayer ... \n\n"
    kill $!
  else
    xterm -e ./reader.x -p data_pendulum -o socket_arduino &
    sleep 1
    ./normalisation.x -p data_pendulum -i socket_arduino --samplenum 10000 > data_normalisation
    kill $!
  fi
fi

if [ "$1" == "tracking" ]; then
  if [ "$2" == "replay" ]; then
    # start replayer in background
    echo -e "\nstarting replayer ...\n\n"
    ./replay.x -f -d -t -i data_values_lastreplay.csv -p data_pendulum -o socket_replay -r 550 > /dev/null &
    
    sleep 0.000000001
    echo -e "\nstarting tracker ...\n\n"
    ./tracker.x -p data_pendulum -n data_normalisation -i socket_replay -o socket_angles --showx11gui --showoverflows --printtempdata > data_tracking_lasttemporary.csv
    
    # killing of replayer not necessary, because the tracker runs
    # til end of data from replayer
  else
    # start arduino reader
    xterm -e ./reader.x -p data_pendulum -o socket_arduino &
    
    sleep 1
    
    ./tracker.x -p data_pendulum -n data_normalisation -i socket_arduino -o socket_angles --showx11gui --showoverflows --printtempdata > data_tracking_lasttemporary.csv
    
  fi
fi

