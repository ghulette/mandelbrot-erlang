#!/usr/bin/env bash

# "Usage: mandel.sh NumWorkers PixelsOnASide Levels"
erl -eval 'dm:test(4,64,64,4)' -noshell
