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

export PGPASSWORD=${DB_CREDS#*:}
DB_CMD="psql -h localhost -U ${DB_CREDS%:*} -d bamboo"

PLAN_SRC=$1
PLAN_DST=$2
# Get quarantined cases from DB
CASES=`echo SELECT TEST_CLASS_NAME, TEST_CASE_NAME, TCL.TEST_CLASS_ID FROM BUILD B JOIN TEST_CLASS TCL ON TCL.PLAN_ID=B.BUILD_ID JOIN TEST_CASE TC ON TC.TEST_CLASS_ID=TCL.TEST_CLASS_ID WHERE B.FULL_KEY LIKE \'${PLAN_SRC}\' AND QUARANTINE_DATE IS NOT NULL \; | ${DB_CMD} | tail -n +3 | grep -v row`
IFS=$'\n'  # Set the input field separator. Necessary to properly iterate $CASES.
count=0
for i in $CASES; do
    IFS=$'\n'  # Set the input field separator. Necessary to properly iterate $CASES.
    SUITE=`echo $i | awk '{print $1}'`
    CASE=`echo $i | awk '{print $3}'`
    TEST_CLASS_ID=`echo $i | awk '{print $5}'`
    IFS=" "
    MASTER_JOB_ID=`echo SELECT MASTER_JOB_ID FROM TEST_CLASS WHERE TEST_CLASS_ID = ${TEST_CLASS_ID} \; | ${DB_CMD} | sed -n '3{s/^[[:space:]]*//; s/[[:space:]]*$//p}'`
    JOB_TITLE=`echo SELECT TITLE FROM BUILD WHERE BUILD_ID = ${MASTER_JOB_ID} \; | ${DB_CMD} | sed -n '3{s/^[[:space:]]*//; s/[[:space:]]*$//p}'`
    TEST_IDS=`echo SELECT TEST_CASE_ID FROM BUILD B JOIN TEST_CLASS TCL ON TCL.PLAN_ID=B.BUILD_ID JOIN TEST_CASE TC ON TC.TEST_CLASS_ID=TCL.TEST_CLASS_ID WHERE B.FULL_KEY LIKE \'${PLAN_DST}\' AND TC.TEST_CASE_NAME = \'${CASE}\' AND TCL.TEST_CLASS_NAME = \'${SUITE}\' \; | ${DB_CMD} | tail -n +3` # | sed -n '3{s/^[[:space:]]*//; s/[[:space:]]*$//p}'
    if [ "${TEST_IDS}"x = "(0 rows)"x ]; then
        echo Warning: Test case not found: SUITE=$SUITE, CASE=$CASE
    fi
    TEST_IDS=`echo "${TEST_IDS}" | grep -v row`
    IFS=$'\n'
    # select the test_id which master_job_id/build has the same title as the job_title above
    for j in $TEST_IDS; do
        IFS=" "
        T=`echo SELECT B.TITLE FROM TEST_CASE TC JOIN TEST_CLASS TCL ON TC.TEST_CLASS_ID=TCL.TEST_CLASS_ID JOIN BUILD B ON TCL.MASTER_JOB_ID=BUILD_ID WHERE TEST_CASE_ID = ${j} \; | ${DB_CMD} | sed -n '3{s/^[[:space:]]*//; s/[[:space:]]*$//p}'`
        if [ "${T}"x = "${JOB_TITLE}"x ]; then
            # Quarantine it
            echo "Will quarantine ${j}: SUITE=${SUITE}, CASE=${CASE}, JOB=${JOB_TITLE}"
            let count=count+1
            res=$(curl -H "Authorization: Bearer ${BAMBOO_TOKEN}" -H "Content-type: application/json" -X POST -d '{"expiryDuration": null}' http://localhost:8085/rest/api/latest/plan/${PLAN_DST}/test/${j#"${j%%[0-9]*}"}/quarantine 2> /dev/null)
            echo res=":$res:" size=${#res}
        fi
    done
done
echo $count test cases were selected for quarantine
