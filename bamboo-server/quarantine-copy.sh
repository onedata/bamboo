#!/bin/bash

# Authors: Darin Nikolow
# Copyright (C) 2022 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# Usage: ./quarantine-copy.sh <PLAN_SRC> <PLAN_DST>
#
# This script looks for quarantined tests in PLAN_SRC and quarantines them
# in PLAN_DST
# The script resides on the bamboo server in /home/ubuntu/bin.
# It needs some credentials which are placed in /home/ubuntu/.bamboo-creds
#
# Example content of .bamboo-creds:
#
#   export DB_CREDS=myuser:mypass
#   export BAMBOO_TOKEN=my_bamboo_token
#
. /home/ubuntu/.bamboo-creds 

PLAN_SRC=$1
PLAN_DST=$2
# Get quarantined cases from DB
CASES=`echo SELECT TEST_CLASS_NAME, TEST_CASE_NAME FROM BUILD B JOIN TEST_CLASS TCL ON TCL.PLAN_ID=B.BUILD_ID JOIN TEST_CASE TC ON TC.TEST_CLASS_ID=TCL.TEST_CLASS_ID WHERE B.FULL_KEY LIKE \'${PLAN_SRC}\' and QUARANTINE_DATE \!= \"\"\; | mysql -u ${DB_CREDS%:*} -p${DB_CREDS#*:} -D bamboo | tail -n +2`
IFS=$'\n'  # Set the input field separator. Necessary to properly iterate $CASES. 
for i in $CASES; do
    SUITE=`echo $i | awk '{print $1}'`
    CASE=`echo $i | awk '{print $2}'`
    TEST_ID=`echo SELECT TEST_CASE_ID FROM BUILD B JOIN TEST_CLASS TCL ON TCL.PLAN_ID=B.BUILD_ID JOIN TEST_CASE TC ON TC.TEST_CLASS_ID=TCL.TEST_CLASS_ID WHERE B.FULL_KEY LIKE \'${PLAN_DST}\' and TC.TEST_CASE_NAME = \'${CASE}\' and TCL.TEST_CLASS_NAME = \'${SUITE}\' \; | mysql -u  ${DB_CREDS%:*} -p${DB_CREDS#*:} -D bamboo | tail -n +2`
    echo TEST_ID=$TEST_ID
    if [ ${TEST_ID}x = x ]; then
        echo Warning: Test case not found: SUITE=$SUITE, CASE=$CASE 
    fi
    curl -H "Authorization: Bearer ${BAMBOO_TOKEN}" -H "Content-type: application/json" -X POST -d '{"expiryDuration": null}' http://localhost:8085/rest/api/latest/plan/${PLAN_DST}/test/${TEST_ID}/quarantine    
done


