#!/bin/bash

if [[ $# -lt 1 ]] ; then
  echo "Too few arguments"; exit 1
fi
start_config=$1
if [[ $# -eq 2 ]] ; then
  end_config=$2
else
  end_config=$start_config
fi

for config_id in $(seq $start_config $end_config) ; do
  sbatch src/hdiv.sbatch $config_id
  # echo $config_id
done
