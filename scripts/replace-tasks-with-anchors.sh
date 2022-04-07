#!/bin/bash
# Author: Darin Nikolow <darnik22@gmail.com>
# Copyright (C) 2022 ACK Cyfronet AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# The script replaces frequently used task definition with yaml anchors

# CAUTION
# Before running the script please define the tasks ancors in the YAML file.
# For example, place '&restart-minikube` in the first occurence of the task
# 'restart minikube'


# clear-env
if grep -w '&clear-env\($\| \)' $1; then
    perl -0777 -i -pe 's/  - script:\n      interpreter: BINSH_OR_CMDEXE\n      scripts:\n      - curl \$\{bamboo.OnedataFinalTasksURL\} \| bash -\n      description: Clear env/  - script: *clear-env/g' $1
fi

# restart-minikube
if grep -w '&restart-minikube\($\| \)' $1; then
    perl -0777 -i -pe 's/  - script:\n      interpreter: BINSH_OR_CMDEXE\n      scripts:\n      - sudo \$\{HOME\}\/restart_minikube.sh\n      description: Restart minikube/  - script: *restart-minikube/g' $1
fi

# force-clean-repo
if grep -w '&force-clean-repo\($\| \)' $1; then
    perl -0777 -i -pe 's/  - script:\n      interpreter: SHELL\n      scripts:\n      - docker run -v \$bamboo_build_working_directory:\/build alpine sh -c '\''rm -rf \/build\/onedata'\''\n      description: Force clean repo/  - script: *force-clean-repo/g' $1
fi

# checkout
if grep -w '&checkout\($\| \)' $1; then
    perl -0777 -i -pe 's/  - checkout:\n      path: onedata\n      force-clean-build: '\''true'\''\n      description: Checkout Default Repository/  - checkout: *checkout/g' $1
fi

# init-submodules
if grep -w '&init-submodules\($\| \)' $1; then
    perl -0777 -i -pe 's/  - script:\n      interpreter: BINSH_OR_CMDEXE\n      scripts:\n      - \|-\n        git remote set-url origin \$\{bamboo.repository.git.repositoryUrl\}\n        git remote -v\n        make submodules\n      working-dir: onedata\n      description: Init submodules/  - script: *init-submodules/g' $1
fi

# download-artifacts
if grep -w '&download-artifacts\($\| \)' $1; then
    perl -0777 -i -pe 's/  - script:\n      interpreter: SHELL\n      scripts:\n      - one_env\/onenv pull_artifacts --hostname \$\{bamboo.artifactRepoHostname\} --port \$\{bamboo.artifactRepoPort\} --packages-only branchConfig.yaml\n      working-dir: onedata\n      description: Download artifacts/  - script: *download-artifacts/g' $1
fi

# parse-test-results
if grep -w '&parse-test-results\($\| \)' $1; then
    perl -0777 -i -pe 's/  - test-parser:\n      type: junit\n      ignore-time: '\''false'\''\n      test-results: onedata\/test-reports\/results\*\.xml\n      description: Parse test results/  - test-parser: *parse-test-results/g' $1
fi

# clear-env-and-working-dir
if grep -w '&clear-env-and-working-dir\($\| \)' $1; then
    perl -0777 -i -pe 's/  - script:\n      interpreter: BINSH_OR_CMDEXE\n      scripts:\n      - curl \$\{bamboo.OnedataFinalTasksURL\} \| bash -\n      working-dir: onedata\n      description: Clear env and working dir/  - script: *clear-env-and-working-dir/g' $1
fi




