#!/bin/bash
# run in res dir

res_dir=$1

# files are of form res1_1.csv
for f in $res_dir/*.csv; do
  # strip off _1.csv
  res=${f%%_*}
  if [ ! "$f" -ef "$res.csv" ] ; then
    if [ ! -f "$res.csv" ] ; then
      head -n 1 $f > $res.csv
    fi
    tail -n +2 $f >> $res.csv
  fi
done

#
#
# if [[ $# -lt 1 ]] ; then { echo "Wrong number of arguments"; exit 1; }; fi
# res_dir=$(realpath "$1")
# _res=$2
#
# function _merge() {
#   if [[ -d $1 ]] ; then
#     if ! [[ ${1##*/} = "old" ]] ; then
#       for x in $1/* ; do
#         _merge $x $2
#       done
#     fi
#   else
#     if [[ -z _res ]]; then res="res"; else res=$_res; fi
#     if ! [ "$1" -ef $res_dir/$res.csv ] ; then
#       if [[ ${1##*/} = ${res}* ]] ; then
#         if [ ! -f $res_dir/$res.csv ] ; then
#           head -n 1 $1 > $res_dir/$res.csv
#         fi
#         tail -n +2 $1 >> $res_dir/$res.csv
#       fi
#     fi
#   fi
# }
#
# _merge $res_dir $res
