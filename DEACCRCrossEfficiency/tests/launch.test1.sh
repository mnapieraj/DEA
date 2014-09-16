#!/bin/sh
cd ..
R --slave --vanilla --args "${PWD}/tests/in1" "${PWD}/tests/out1" < main.R 
