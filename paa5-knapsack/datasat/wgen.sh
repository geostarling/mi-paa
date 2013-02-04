#!/bin/bash


for file in `ls`; do
LINE=""
  for i in {1..20}; do
	LINE="$LINE $(( ($RANDOM % 100) + 1))"
  done
 echo $file;
 echo $LINE >> $file
done
86 55 92 75 50 81 70 22 40 6 63 42 81 53 85 40 31 34 1 41
