#!/bin/bash

# Authors: Darin Nikolow
# Copyright (C) 2022 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

. /home/ubuntu/.bamboo-creds

PLAN_TO_RUN=$1
PLAN_TO_CHECK=$2

STATUS='NOT_FINISHED'
while [ $STATUS = 'NOT_FINISHED' ]; do
    S=`${BAMBOO_CLI} -a getBuild --build ${PLAN_TO_CHECK} --server https://bamboo.onedata.org --user ${BAMBOO_CREDS%:*} --password ${BAMBOO_CREDS#*:} | grep '^State'`
    if [ $? -eq 0 ]; then
	if echo $S | grep 'UNKNOWN'; then
	    sleep 30
	else
	    STATUS='FINISHED'
	    echo finished
	fi
    else
	echo Connection error
	sleep 30
    fi
done
sleep 30
${BAMBOO_CLI} -a queueBuild --build ${PLAN_TO_RUN} --server https://bamboo.onedata.org --user ${BAMBOO_CREDS%:*} --password ${BAMBOO_CREDS#*:}



