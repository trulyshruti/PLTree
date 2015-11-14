#!/bin/bash

#
# This script tests .tree files. 
# usage: ./tester.sh FILE.tree
# To run, make sure that:
# - FILE.tree is in same directory
# - output_out/ directory exists in same directory
# - output_exp/FILE_exp file exists (expected output file)
# - calc exists in same directory
#

# define suffixes
out='_out'
exp='_exp'

# define directories
outdict='output_out/'
expdict='output_exp/'

# Make sure there's only one argument
if ! [ $# -eq 1 ]
then
	echo 'usage: ./tester.sh FILE.tree'
	exit 2;
# Make sure file is in directory
elif ! [ -a ${1} ]
then
	echo 'Place FILE.tree in this directory.'
	exit 2;
# Make sure calc is in directory
elif ! [ -a calc ]
then
	echo 'Invoke lexer/parser to produce calc file.'
	exit 2;
# Make sure an expected output file is present
elif ! [ -a ${expdict}${1%.*}${exp} ]
then
	echo 'Place FILE_exp file in output_exp/ directory.'
	exit 2;
else
	# remove .tree extension
	progname="${1%.*}"
	# run parser on input file; save output
	cat $1 | ./calc > "${outdict}${progname}${out}"

	# check if there are diffs
	if ! [[ $(diff ${outdict}${progname}${out} ${expdict}${progname}${exp}) ]]
	then
		echo 'NO DIFF FOUND'
		exit 0;
	else
		echo 'DIFFS DETECTED'
		echo '-------------------------'
		echo 'view with: '
		echo '(1) diff (default)'
		echo '(2) diff (context)'
		echo '(3) vim -d'
		echo 'enter [1,2,3] and press [ENTER]: '
		read choice
		echo -e "\n"
		if [ $choice -eq 1 ]
		then
			diff ${outdict}${progname}${out} ${expdict}${progname}${exp}
		elif [ $choice -eq 2 ]
		then
			diff -c ${outdict}${progname}${out} ${expdict}${progname}${exp}
		elif [ $choice -eq 3 ]
		then
			vim -d ${outdict}${progname}${out} ${expdict}${progname}${exp}
		else
			echo 'invalid choice. exiting.'
			exit 2;
		fi
	fi
fi
