#!/bin/bash
read -p "Enter range of number: " range
range="1..$range"
for file in `ls`; do
LINE=""
  for i in $(eval echo {$range}); do
	LINE="$LINE $(( ($RANDOM % 100) + 1))"
  done
 echo $file;
 echo "%" >> $file
 echo "0" >> $file
 echo "" >> $file
 echo $LINE >> $file
done

