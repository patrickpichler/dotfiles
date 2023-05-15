#!/bin/bash

escaped=`(sed 's/"/\\"/' | sed -En 's/\s?-e\s(.*)$/-e "\1"/p') <<< $@`

eval "termite $escaped"
