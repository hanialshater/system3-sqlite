#!/bin/sh
# usage: sh scratch/all.sh [verbose]
cd /tmp/claude-0/-home-user/aa2da695-733d-5610-822b-d0f420a6080f/scratchpad/b13-runs/arms/O-m-planner/V
for f in t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16; do
  if [ -f scratch/$f.sql ]; then
    if [ -n "$1" ]; then python3 -I scratch/cmp.py scratch/$f.sql 2>&1; else echo $f $(python3 -I scratch/cmp.py scratch/$f.sql 2>&1 | tail -1); fi
  fi
done
