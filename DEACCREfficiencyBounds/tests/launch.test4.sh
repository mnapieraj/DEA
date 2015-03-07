#!/bin/sh
cd ..
R --slave --vanilla --args "${PWD}/tests/in4" "${PWD}/tests/out4" < main.R 
