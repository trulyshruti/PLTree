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

# Make sure there's arguments
if [ $# -eq 0 ]
then
	echo 'usage: ./tester.sh [FILE.tree | treedir/]?'
	exit 2;
# Make sure calc is in directory
elif ! [ -a calc ]
then
	echo 'Invoke lexer/parser to produce calc file.'
	exit 2;
else
	for arg
	do
		# if it's a regular file
		if [ -f $arg ]
		then
			# remove .tree extension
			progname="${arg%.*}"
			# Make sure an expected output file is present
			if ! [ -a ${expdict}${progname}${exp} ]
			then
				echo "Place ${progname}_exp file in output_exp/ directory."
				exit 2;
			else
				# run parser on input file; save output
				cat $arg | ./calc > "${outdict}${progname}${out}"

				# check if there are diffs
				if ! [[ $(diff -bw ${outdict}${progname}${out} ${expdict}${progname}${exp}) ]]
				then
					printf "${progname}: \033[0;32mSUCCESS\033[0m\n"
				else
					printf "${progname}: \033[0;31mFAILED\033[0m\n"
				fi
			fi
		# if it's a directory
		elif [ -d $arg ]
		then
			for f in $(ls $arg)
			do
				# remove .tree extension
				progname="${f%.*}"
				# Make sure an expected output file is present
				if ! [ -a ${expdict}${progname}${exp} ]
				then
					echo "Place ${progname}_exp file in output_exp/ directory."
					exit 2;
				else
					# run parser on input file; save output
					cat $arg/$f | ./calc > "${outdict}${progname}${out}"

					# check if there are diffs
					if ! [[ $(diff -bw ${outdict}${progname}${out} ${expdict}${progname}${exp}) ]]
					then
						printf "${arg}/${progname}: \033[0;32mSUCCESS\033[0m\n"
					else
						printf "${arg}/${progname}: \033[0;31mFAILED\033[0m\n"
					fi
				fi
			done
		fi
	done
fi
