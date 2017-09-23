#!/bin/bash

# files are of form res_1_1.csv
for f in res/*.csv; do
  # strip off _1_1.csv
  res=${f%%_*}
  if [ ! "$f" -ef "$res.csv" ] ; then
    if [ ! -f "$res.csv" ] ; then
      head -n 1 $f > $res.csv
    fi
    tail -n +2 $f >> $res.csv
  fi
done
