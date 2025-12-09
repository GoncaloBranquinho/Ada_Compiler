#!/bin/sh

cd src
make clean >/dev/null 2>&1
make build
cd ../scripts/

tests=0
successes=0
fails=0

rm -f ../examples/*AST.txt ../examples/*Table.txt ../examples/*IR.txt ../examples/*Addresses.txt ../examples/*Mips.txt


for filename in ../examples/*.adb; do
  tests=$((tests + 1))
  ada_bin="../src/bin/ada"

  if $ada_bin "$filename" 2>&1; then
    echo -e "\033[0;32m$filename Success\033[0m"
    successes=$((successes + 1))
  else
    echo -e "\033[0;31m$filename Failed\033[0m"
    fails=$((fails + 1))
  fi
done

echo -e -n Tests:$tests
echo -e -n " |\033[0;32m Success:$successes\033[0m |"
echo -e "\033[0;31m Fail:$fails\033[0m"

