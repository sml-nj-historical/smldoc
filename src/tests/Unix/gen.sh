#!/bin/sh
#
# run the smldoc tool on the test files
#

echo "sml @SMLcmd=smldoc@SMLload=../../smldoc *.sml"
sml @SMLcmd=smldoc @SMLload=../../smldoc *.sml

