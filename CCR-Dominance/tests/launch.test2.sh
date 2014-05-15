#!/bin/sh
cd ..
R --slave --vanilla --args "${PWD}/tests/in2" "${PWD}/tests/out2" < main.R 
