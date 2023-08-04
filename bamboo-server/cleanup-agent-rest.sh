#!/bin/bash

# Authors: Darin Nikolow
# Copyright (C) 2022 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# Usage: ./cleanup-agent-rest.sh <AGENT_TO_CLEAN> 
# 
# This script cleanups the specified agent.
# The script resides on the bamboo server in /home/ubuntu/bin.
#
# .bamboo-creds contains the necessary credentials. Example content:
#
#   export BAMBOO_CREDS=bamboo_user:password
#   export BAMBOO_TOKEN=my_bamboo_token
#   export BAMBOO_CLI='/home/ubuntu/ACLI/acli bamboo'
#

{
    source ~/agent
    source ~/.bamboo-creds
    ANSIBLE_BAMBOO=/home/ubuntu/ansible-bamboo
    AGENT_NO_SPACE=`echo $1 | cut -d' ' -f1`
    SUMMARY_LOG=/tmp/cleanup-summary.log
    AGENT_STATUS_LOG=/tmp/agent-status.log
    echo `date` Cleaning $1... >> ${SUMMARY_LOG}
    if [ ${CLEANUP_AGENT_FORCE}" " != "yes " ]; then
	ENABLED=`curl -s -u $BAMBOO_CREDS http://localhost:8085/rest/api/latest/agent | jq '.[] | select(.name == "'${AGENT_NO_SPACE}'")|.enabled'`
	if [ ${ENABLED}X == "falseX" ]; then
	    echo `date` Agent $1 is not enabled. Exiting...
	    echo `date` Agent $1 is not enabled. Exiting... >> ${SUMMARY_LOG}
	    exit -1
	fi
	if [ ${ENABLED}X != "trueX" ]; then
	    echo `date` Error
	    echo `date` "The Cleanup of $1 ended with error (Checking agent ststus)." >> ${SUMMARY_LOG}
	    exit -1
	fi	    
    fi
    echo `date` Disabling agent $1
    ID=`curl -s -u $BAMBOO_CREDS http://localhost:8085/rest/api/latest/agent | jq '.[] | select(.name == "'${AGENT_NO_SPACE}'")|.id'`
    RES=`curl -s -u $BAMBOO_CREDS -X PUT http://localhost:8085/rest/api/latest/agent/${ID}/disable`
    if [ `echo ${RES} | jq .enabled`"X" != "falseX" ]; then
	echo `date` Error
	echo `date` "The Cleanup of $1 ended with error (Disabling agent)." >> ${SUMMARY_LOG}
	exit -1
    fi
    echo `date` Waiting for $1 to become idle
    count=0
    while [ `curl -s -u $BAMBOO_CREDS http://localhost:8085/rest/api/latest/agent/${ID} | jq .general.busy`X == "trueX" ]; do 
	sleep 30
	let count+=30
	printf "\r$1 still not idle - $count seconds elapsed"
    done
    echo `date` Exit status was $?
    flock ${AGENT_STATUS_LOG} echo `date +%s` ${AGENT_NO_SPACE} DISABLED >> ${AGENT_STATUS_LOG}
    echo `date` Cleanup with ansible
    cd ${ANSIBLE_BAMBOO}
    ansible-playbook -i hosts -l ${AGENT_NO_SPACE} -f 20 -T 50 bamboo-cleanup-minikube.yml
    if [ $? -ne 0 ]; then
	echo `date` Error
	echo `date` "The Cleanup of $1 ended with error (ansible)." >> ${SUMMARY_LOG}
	exit -1
    fi
    # Check for errors
    echo `date` Enabling agent $1
    RES=`curl -s -u $BAMBOO_CREDS -X PUT http://localhost:8085/rest/api/latest/agent/${ID}/enable`
    if [ `echo ${RES} | jq .enabled`"X" != "trueX" ]; then
	echo `date` Error
	echo `date` "The Cleanup of $1 ended with error (Enabling agent)." >> ${SUMMARY_LOG}
	exit -1
    fi
    flock ${AGENT_STATUS_LOG} echo `date +%s` ${AGENT_NO_SPACE} ENABLED >> ${AGENT_STATUS_LOG}
    echo `date` $1 is clean. >> ${SUMMARY_LOG}
} >> /tmp/cleanup.log 2>&1

