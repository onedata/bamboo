#!/bin/bash
###-------------------------------------------------------------------
### @author Lukasz Opiola
### @copyright (C) 2021 ACK CYFRONET AGH
### This software is released under the MIT license
### cited in 'LICENSE.txt'.
### @end
###-------------------------------------------------------------------
### @doc
### This script iterates through a static list of all repos that include bamboos
### as a submodule and bumps it to the HEAD of given branch, creating a commit
### and pushing the changes. The list of repos must be manually maintained -
### whenever a new repo with bamboos is added, it should be added here too.
### Can be run for the develop branch, in such case it requires permissions to
### push changes directly to develop (intended to be run within a bamboo
### automated job).
### @end
###-------------------------------------------------------------------

set -e

USER_NAME=${3:-`git config user.name`}
USER_NAME=${USER_NAME:-"Bamboo Agent"}
USER_EMAIL=${4:-`git config user.email`}
USER_EMAIL=${USER_EMAIL:-"bamboo@bamboo.onedata.org"}

ALL_REPOS=(
    appmock
    cluster-manager
    cluster-worker
    ctool
    fs-onedatafs
    gui
    helpers
    homepage
    ioreplay
    macaroons
    oneclient-pkg
    onedata-acceptance
    onedatafs-jupyter
    onenv-ct
    onepanel
    onepanel-gui
    onepanel-swagger
    oneprovider-gui
    oneprovider-pkg
    oneprovider-swagger
    onezone-gui
    onezone-gui-plugin-ecrin
    onezone-pkg
    onezone-swagger
    op-worker
    oz-worker
    rtransfer_link
)

TARGET_BRANCH_NAME=${1}
if [ -z "${TARGET_BRANCH_NAME}" ]; then
    echo "Please provide the target branch name in the first argument. "
    echo "The branch will be created (if non-existent) in all repos and the "
    echo "bamboos submodule will be checked out to HEAD of this branch."
    exit 1
fi

BASE_BRANCH_NAME=${2}
if [ -z "${BASE_BRANCH_NAME}" ]; then
    echo "Please provide the base branch name in the second argument."
    echo "If the target branch needs to be created, it will be based off "
    echo "this branch."
    exit 1
fi

echo "Bumping bamboos submodule to HEAD of branch '${TARGET_BRANCH_NAME}' in all repos!"

WORK_DIR="$(mktemp -d)"
echo "Entering a tmp working directory: ${WORK_DIR}"
cd ${WORK_DIR}

for REPO in ${ALL_REPOS[@]}; do
    echo " "
    echo "-------------------------------------------------------------------------"
    echo " "
    git clone ssh://git@git.onedata.org:7999/vfs/${REPO}.git
    cd ${REPO}
    git checkout ${BASE_BRANCH_NAME}
    git checkout ${TARGET_BRANCH_NAME} || git checkout -b ${TARGET_BRANCH_NAME}
    git submodule init bamboos || : # do not fail here - bamboos might be set to an invalid ref
    git submodule update bamboos || : # do not fail here - bamboos might be set to an invalid ref
    cd bamboos
    git checkout ${TARGET_BRANCH_NAME}
    cd ..
    git add bamboos
    git -c user.name="${USER_NAME}" -c user.email="${USER_EMAIL}" commit -m "Update bamboos to origin/${TARGET_BRANCH_NAME}" || echo "already on origin/${TARGET_BRANCH_NAME}"
    git push origin ${TARGET_BRANCH_NAME}
    cd ..
done

rm -rf "${WORK_DIR}" || echo "Warning: cannot remove the tmp working directory: ${WORK_DIR}"
