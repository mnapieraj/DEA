#!/bin/sh
cd ..
R --slave --vanilla --args "${PWD}/tests/in3" "${PWD}/tests/out3" < main.R 
