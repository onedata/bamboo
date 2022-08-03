#!/bin/bash

# Authors: Darin Nikolow
# Copyright (C) 2022 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# Usage: ./run-build-after.sh <PLAN_TO_RUN> <PLAN_TO_CHECK>
# 
# This script waits until the build of PLAN_TO_CHECK finishes and after
# that runs a new build of PLAN_TO_RUN.
# The script resides on the bamboo server in /home/ubuntu/bin.
#
# .bamboo-creds contains the necessary credentials. Example content:
#
#   export BAMBOO_CREDS=bamboo_user:password
#   export BAMBOO_TOKEN=my_bamboo_token
#   export BAMBOO_CLI='/home/ubuntu/ACLI/acli bamboo'
#
. /home/ubuntu/.bamboo-creds

PLAN_TO_RUN=$1
PLAN_TO_CHECK=$2

STATUS='NOT_FINISHED'
while [ $STATUS = 'NOT_FINISHED' ]; do
    S=`${BAMBOO_CLI} -a getBuild --build ${PLAN_TO_CHECK} --server https://bamboo.onedata.org --user ${BAMBOO_CREDS%:*} --password ${BAMBOO_CREDS#*:} | grep '^State'`
    if [ $? -eq 0 ]; then
        if echo $S | grep 'UNKNOWN'; then
            sleep 30         # Interval between subsequent getBuild requests
        else
            STATUS='FINISHED'
            echo finished
        fi
    else
        echo Connection error
        sleep 30             # Time to wait before retrying next request in case of connectivity problems
    fi
done
sleep 30                     # Cooldown time to let bamboo finish its operation regarding the build, e.g., updating DB records
${BAMBOO_CLI} -a queueBuild --build ${PLAN_TO_RUN} --server https://bamboo.onedata.org --user ${BAMBOO_CREDS%:*} --password ${BAMBOO_CREDS#*:}



