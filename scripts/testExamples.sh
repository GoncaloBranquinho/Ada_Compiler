#!/bin/sh

if [ "$1" != "-e" ] || [ ! -f "src/bin/ada" ]; then
    echo "Recompiling project..."
    cd src
    make clean >/dev/null 2>&1
    make build
    cd ../scripts/
else
    cd scripts
fi

tests=0
successes=0
fails=0

for dir in ../examples/*/; do

  rm -f "$dir"*.AST.txt "$dir"*.Table.txt "$dir"*.IR.txt "$dir"*.Addresses.txt "$dir"*.Mips.txt

  for filename in "$dir"*.adb; do
    [ -e "$filename" ] || continue 
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
done

echo -e -n Tests:$tests
echo -e -n " |\033[0;32m Success:$successes\033[0m |"
echo -e "\033[0;31m Fail:$fails\033[0m"

