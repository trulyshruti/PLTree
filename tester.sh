#!/bin/bash

#
# This script tests .tree files. 
# usage: ./tester.sh [-c] [FILE.tree | treedir/]?
#

# define suffixes
out='_out'
exp='_exp'
# define directories
outdict='tests/output_out/'
expdict='tests/output_exp/'
# define default option
options='./calc'

# Make sure there's arguments
if [ $# -eq 0 ]
then
	echo 'usage: ./tester.sh [FILE.tree | treedir/]?'
	exit 2;
fi

make_calc() {
	# run "make"; surpress output
	make &> /dev/null
	# double check that calc was created
	if ! [ -a calc ]
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
			options='./calc -c'
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
						cat $arg | $options > "${outdict}${progname}.c"
						gcc -Wall -c "${outdict}${progname}.c" -o "${outdict}${progname}.o"
						gcc -Wall -c tree.c -o "${outdict}tree.o"
						gcc -Wall -o "${outdict}${progname}" "${outdict}${progname}.o" "${outdict}tree.o"
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
			exit 2;
			compile () {
				gcc -Wall -c tests/programs/hello.c -o tests/programs/hello.o
				gcc -Wall -c tests/programs/tree.c -o tests/programs/tree.o 
				gcc -Wall -o tests/programs/hello tests/programs/hello.o tests/programs/tree.o

			}
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
