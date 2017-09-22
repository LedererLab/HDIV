#!/bin/bash

# if [[ $# -lt 1 ]] ; then { echo "Wrong number of arguments"; exit 1; }; fi
if [[ ! -d err ]] ; then mkdir err ; fi
if [[ ! -d out ]] ; then mkdir out ; fi
if [[ ! -d res ]] ; then mkdir res ; fi

nconfig=$(($(< configs/configs.csv wc -l) - 1))
for config_id in $(seq 1 $nconfig) ; do
  if [[ -f src/configure.r ]] ; then
    if [[ ! -d configs/$config_id ]] ; then
      mkdir configs/$config_id
    fi
    sbatch src/configure.sbatch $config_id
  fi
done
