#!/bin/bash
# Author: Lukasz Opiola
# Copyright (C) 2021 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# This scripts checks the bamboo variables concerning coverage and returns
# an exit code indicating if coverage procedures should be skipped:
#    0 - coverage was disabled, all procedures concerning coverage should be skipped
#    1 - coverage was enabled during this run and cover artifacts should be collected and processed
#
# These bamboo variables are also analyzed in the ct_run.py and ct_onenv.py scripts to determine
# if coverage should be enabled during CT tests.

if [[ "${bamboo_coverOptionOverride}" == "true" ]]; then
    exit 1
elif [[ "${bamboo_coverOptionOverride}" == "false" ]]; then
    exit 0
elif [[ "${bamboo_coverOptionOverride}" == "develop_only" ]]; then
    if [[ "${bamboo_planRepository_branchName}" == "develop" ]]; then
        exit 1
    else
        exit 0
    fi
else
    exit 1
fi