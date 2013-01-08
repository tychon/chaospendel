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
  if [ "$2" == "fastreplay" ]; then
    # start replayer in background
    echo -e "\nstarting replayer ...\n\n"
    ./replay.x -f -d -i data_values_lastreplay.csv -p data_pendulum -o socket_replay -r 100000 > /dev/null &
    
    sleep 0.00000001
    echo -e "\nstarting tracker ...\n\n"
    ./tracker.x -p data_pendulum -n data_normalisation -i socket_replay -o socket_angles --showoverflows --printtempdata > data_tracking_lasttemporary.csv
  
  elif [ "$2" == "replay" ]; then
    # start replayer in background
    echo -e "\nstarting replayer ...\n\n"
    ./replay.x -f -d -i data_values_lastreplay.csv -p data_pendulum -o socket_replay -r 550 > /dev/null &
    
    sleep 0.1
    
    echo -e "\nstarting tracker ...\n\n"
    ./tracker.x -p data_pendulum -n data_normalisation -i socket_replay -o socket_angles --showx11gui --showoverflows --printtempdata > data_tracking_lasttemporary.csv
  
  else
    echo -e "\nstarting arduino reader ...\n\n"
    xterm -e ./reader.x -p data_pendulum -o socket_arduino &
    
    sleep 1
    
    ./tracker.x -p data_pendulum -n data_normalisation -i socket_arduino -o socket_angles --showx11gui --maxframerate 50 --showoverflows --printtempdata > data_tracking_lasttemporary.csv
  fi
fi

if [ "$1" == "prediction" ]; then
  echo -e "\nstarting arduino reader ...\n\n"
  xterm -e ./reader.x -p data_pendulum -o socket_arduino &
  
  sleep 0.1
  
  echo -e "\nstarting tracker ...\n\n"
  xterm -e ./tracker.x -p data_pendulum -n data_normalisation -i socket_arduino -o socket_angles --showoverflows &
  
  sleep 0.1
  
  echo -e "\nstarting markov prediction ...\n\n"
  ./markov_prediction.x -p data_pendulum -i socket_angles
  
fi


