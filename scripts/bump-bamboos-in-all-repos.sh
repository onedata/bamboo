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
###
### Can be run for the develop branch, in such case it requires permissions to
### push changes directly to develop (intended to be run within a bamboo
### automated job).
###
### By default, the script will bump the bamboos ref to the HEAD of
### `TARGET_BRANCH_NAME` (1st argument) only in repos for which the branch
### with the same name exists - otherwise, they will be skipped.
### Using the `CREATE_MISSING_BRANCHES` and `BASE_BRANCH_NAME` env variables,
### it is possible to automatically create the branch `TARGET_BRANCH_NAME` in
### repos where it does not exist.
### @end
###-------------------------------------------------------------------

set -e

USER_NAME=${2:-$(git config user.name)}
USER_NAME=${USER_NAME:-"Bamboo Agent"}
USER_EMAIL=${3:-$(git config user.email)}
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
    onedata-documentation
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

# In case the target branch does not exist in a repo, it will be automatically
# created if this variable is set to true.
CREATE_MISSING_BRANCHES=${CREATE_MISSING_BRANCHES:-false}
# If the target branch is to be created, it will be based off this branch.
BASE_BRANCH_NAME=${BASE_BRANCH_NAME:-"develop"}

echo "Bumping bamboos submodule to HEAD of branch '${TARGET_BRANCH_NAME}' in all repos!"

WORK_DIR="$(mktemp -d)"
echo "Entering a tmp working directory: ${WORK_DIR}"
cd "${WORK_DIR}"

for REPO in "${ALL_REPOS[@]}"; do
    echo " "
    echo "-------------------------------------------------------------------------"
    echo " "
    git clone "ssh://git@git.onedata.org:7999/vfs/${REPO}.git"
    cd "${REPO}"

    git checkout "${TARGET_BRANCH_NAME}" || {
        if [ "$CREATE_MISSING_BRANCHES" = false ] ; then
            echo ""
            echo "[SKIPPED] Target branch '${TARGET_BRANCH_NAME}' does not exist in repo '${REPO}' - skipping!"
            echo "Consider using 'CREATE_MISSING_BRANCHES' and 'BASE_BRANCH_NAME' env variables to automatically create missing branches."
            cd ..
            continue
        else
            git checkout "${BASE_BRANCH_NAME}" || {
                echo ""
                echo "[ERROR] Cannot create target branch from base branch '${BASE_BRANCH_NAME}' as it does not exist in repo '${REPO}'"
                exit 1 ;
            }
            echo ""
            echo "[INFO] Creating target branch '${TARGET_BRANCH_NAME}' from base branch '${BASE_BRANCH_NAME}'"
            echo ""
            git checkout -b "${TARGET_BRANCH_NAME}"
        fi ;
    }

    git submodule init bamboos || : # do not fail here - bamboos might be set to an invalid ref
    git submodule update bamboos || : # do not fail here - bamboos might be set to an invalid ref
    cd bamboos
    git checkout "${TARGET_BRANCH_NAME}"
    cd ..
    git add bamboos
    git -c user.name="${USER_NAME}" -c user.email="${USER_EMAIL}" commit -m "Update bamboos to origin/${TARGET_BRANCH_NAME}" || echo "already on origin/${TARGET_BRANCH_NAME}"
    git push origin "${TARGET_BRANCH_NAME}"

    echo ""
    echo "[OK] Repo '${REPO}' done!"
    cd ..
done

rm -rf "${WORK_DIR}" || echo "Warning: cannot remove the tmp working directory: ${WORK_DIR}"
