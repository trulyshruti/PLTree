#!/bin/bash

#
# This script tests .tree files. 
# usage: ./tester.sh [-c] [FILE.tree | treedir/]?
# the '-c' flag compiles the program and compares expected output
#
# File structure:
# 	.tree files go in tests/programs
# 	expected AST files go in tests/output_exp/FILE_exp
# 	expected compiled output files go in tests/output_res/FILE_res
# All other directories are only used by the script
#

# define suffixes
out='_out'
exp='_exp'
# define directories
progdict='tests/programs/'
outdict='tests/output_out/'
expdict='tests/output_exp/'
# define default option
options='./pltree'

# Make sure there's arguments
if [ $# -eq 0 ]
then
	expdict='tests/output_res/'
	exp='_res'
	for f in $(ls $progdict)
		do
		# remove .tree extension
		progname="${f%.*}"
		# Make sure an expected output file is present
		if ! [ -a ${expdict}${progname}${exp} ]
		then
			echo "Place ${progname}${exp} file in ${expdict} directory."
			exit 2;
		else
			# make all the files and save output
			make $progdict$progname &> /dev/null
			$progdict$progname > "${outdict}${progname}${out}"

			# check if there are diffs
			if ! [[ $(diff -bw ${outdict}${progname}${out} ${expdict}${progname}${exp}) ]]
			then
				printf "${progdict}${progname}: \033[0;32mSUCCESS\033[0m\n"
			else
				printf "${progdict}${progname}: \033[0;31mFAILED\033[0m\n"
			fi

			# clean directory of all non-.tree files
			find $progdict ! -name "*.tree" -type f -delete
		fi
	done
fi

make_calc() {
	# run "make"; surpress output
	make install &> /dev/null
	# double check that calc was created
	if ! [ -a pltree ]
	then
		echo 'Invoke lexer/parser to produce calc file.'
		exit 2;
	fi
}

# First, check for -c (compile) flag
while getopts ":c" opt; do
	case $opt in
		c)
			# change variables if the -c flag is present
			outdict='tests/output_com/'
			expdict='tests/output_res/'
			rundict='tests/output_run/'
			out='_run'
			exp='_res'
			options='./pltree'
			make_calc
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
						echo "Place ${progname}${exp} file in ${expdict} directory."
						exit 2;
					else
						# compile given file; save output
						$options $arg "${outdict}${progname}.c"
						gcc -Wall "${outdict}${progname}.c" -o "${outdict}${progname}"
						rm -rf *.tmp
						touch "${rundict}${progname}${out}"
						"./${outdict}${progname}" > "${rundict}${progname}${out}"

						# check if there are diffs
						if ! [[ $(diff -bw ${rundict}${progname}${out} ${expdict}${progname}${exp}) ]]
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
							echo "Place ${progname}${exp} file in ${expdict} directory."
							exit 2;
						else
							# compile given file; save output
							$options $arg/$f  "${outdict}${progname}.c"
							gcc -Wall "${outdict}${progname}.c" -o "${outdict}${progname}"
							rm -rf $arg/*.tmp
							touch "${rundict}${progname}${out}"
							"./${outdict}${progname}" > "${rundict}${progname}${out}"

							# check if there are diffs
							if ! [[ $(diff -bw ${rundict}${progname}${out} ${expdict}${progname}${exp}) ]]
							then
								printf "${arg}${progname}: \033[0;32mSUCCESS\033[0m\n"
							else
								printf "${arg}${progname}: \033[0;31mFAILED\033[0m\n"
							fi
						fi
					done
				fi
			done
			exit 2;
			;;

		\?)
			# exit on invalid flag
			echo "Invalid option: -$OPTARG"
			exit 2;
			;;
	esac
done

make_calc

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
				echo "Place ${progname}${exp} file in ${expdict} directory."
				exit 2;
			else
				# run parser on input file; save output
				cat $arg | $options > "${outdict}${progname}${out}"

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
					echo "Place ${progname}${exp} file in ${expdict} directory."
					exit 2;
				else
					# run parser on input file; save output
					cat $arg/$f | $options > "${outdict}${progname}${out}"

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
