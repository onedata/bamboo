#!/bin/bash

# Authors: Darin Nikolow
# Copyright (C) 2022 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# Usage: ./run-build.sh <PLAN_TO_RUN> 
# 
# This script runs a new build of PLAN_TO_RUN.
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

${BAMBOO_CLI} -a queueBuild --build ${PLAN_TO_RUN} --server https://bamboo.onedata.org --user ${BAMBOO_CREDS%:*} --password ${BAMBOO_CREDS#*:}



