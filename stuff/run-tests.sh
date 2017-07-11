#!/bin/sh

#set -e

TPL=~/trealla/tpl

TMP=tmp.out

trap "rm -f $TMP" EXIT

for source in tests/*.pl
do
        expected_output="${source%.*}.expected"

        if [ -f "$expected_output" ]
        then
                echo "Running $source ..."

                if grep -q ":- initialization" $source
                then
                        $TPL --ns -q $source >$TMP
                else
                        $TPL --ns -g main $source >$TMP
                fi

                diff "$expected_output" $TMP
        fi
done
