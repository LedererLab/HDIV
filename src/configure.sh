#!/bin/bash

# if [[ $# -lt 1 ]] ; then { echo "Wrong number of arguments"; exit 1; }; fi
if [[ ! -d err ]] ; then { mkdir err; }; fi
if [[ ! -d out ]] ; then { mkdir out; }; fi

nconfig=$(($(< config/configs.csv wc -l) - 1))
for config_id in $(seq 1 $nconfig) ; do
  if [[ ! -d res ]] ; then mkdir res fi
  if [[ -f config/configure.r ]] ; then
    if [[ ! -d config/$config_id ]] ; then
      mkdir config/$config_id
    fi
    sbatch config/config.sbatch config $config_id
  fi
done
