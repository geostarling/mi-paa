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
71 75 75 66 29 49 92 71 25 70 37 71 2 55 52 51 19 62 67 16
86 52 89 87 90 25 8 84 72 64 16 15 71 28 52 54 11 64 28 32
