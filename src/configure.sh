#!/bin/bash

# if [[ $# -lt 1 ]] ; then { echo "Wrong number of arguments"; exit 1; }; fi
# project_dir=$(realpath $1)
# _project=${project_dir##*src}
# project=${_project#/}
# res_dir=${project_dir%%/src*}/res
# if [[ ! -d $res_dir/$project ]] ; then
#   mkdir $res_dir/$project
# fi
if [[ ! -d err ]] ; then { mkdir err; }; fi
if [[ ! -d out ]] ; then { mkdir out; }; fi

config_dir=src/config
nconfig=$(($(< $config_dir/configs.csv wc -l) - 1))
echo $config_dir
for config_id in $(seq 1 $nconfig) ; do
  if [[ ! -d res ]] ; then mkdir res fi
  if [[ -f $config_dir/configure.r ]] ; then
    if [[ ! -d $config_dir/$config_id ]] ; then
      mkdir $config_dir/$config_id
    fi
    sbatch $config_dir/config.sbatch $config_dir $config_id
  fi
done
