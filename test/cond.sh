#!/usr/bin/env bash
if [[ true -eq 1 ]]; then
	echo "true"
else
	echo "false"
fi

if [[ false -eq 1 ]]; then
	echo "false"
else
	echo "true"
fi

if [[ false -eq 1 ]]; then
	echo "false"
elif [[ false -eq 0 ]]; then
	echo "true"
else
	echo "false"
fi

if [[ false -eq 1 ]]; then
	echo "false"
elif [[ true -eq 0 ]]; then
	echo "false"
else
	echo "true"
fi
