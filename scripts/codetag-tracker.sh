#!/usr/bin/env bash

###-------------------------------------------------------------------
### @author Lukasz Opiola
### @copyright (C) 2020 ACK CYFRONET AGH
### This software is released under the MIT license
### cited in 'LICENSE.txt'.
### @end
###-------------------------------------------------------------------
### @doc
### This script looks for all forgotten fixmes and todos in CWD and dumps them
### to stdout with exit code 1. If there are none, exits with 0.
###
### See the print_failure_summary() function for details how it works.
### @end
###-------------------------------------------------------------------

SCRIPT_NAME=`basename "$0"`
OUTPUT_FILE="$(mktemp)"


IGNORE_LINE_TAG='@codetag-tracker-ignore'


EXCLUDED_DIRS=(
    _build  # do not recurse into the _build directory as it is traversed selectively
    logs
    .git
    .idea
)
EXCLUDED_FILES=(
    ${SCRIPT_NAME}
    add-error.sh
    CHANGELOG.md
)
# list of third party deps that we do not want to scan as we cannot fix the fixmes there
EXCLUDED_THIRD_PARTY_DEPS=(
    base64url
    bp_tree
    cowboy
    cowlib
    cberl
    edown
    esaml
    erldns
    exometer_core
    exometer_lager
    gen_smtp
    goldrush
    hackney
    hut
    jiffy
    jsx
    lager
    locus
    meck
    observer_cli
    proper
    ranch
    recon
    setup
    worker_pool
    yamerl
)


print_failure_summary() {
    echo "Oh no! Found some forgotten fixmes, todos or forbidden functions!"
    echo "---------------------------------------------------------------------"
    echo "Please keep in mind the following guidelines:"
    echo " * fixme         - not tolerated at all, use it to mark places in your code"
    echo "                   that must be fixed before it can make it to production"
    echo "                   (this script will subtly keep an eye on you)"
    echo " "
    echo " * writeme       - same as fixme"
    echo " "
    echo " * todo          - tolerated only if a string matching 'VFS-\\d+' is found in"
    echo "                   the same line, but NOT tolerated if the todo concerns the"
    echo "                   current git branch (well, this is exactly the right moment"
    echo "                   to resolve such todos)."
    echo " "
    echo " * note           - tolerated, can be used to leave a note for the future,"
    echo "                   when a todo with a concrete VFS tag is not viable. "
    echo "                   Do not overuse!"
    echo " "
    echo " * rpc:multicall - not tolerated due to a bug in Erlang OTP that may cause"
    echo "                   a complete VM crash, use utils:rpc_multicall/4,5 from"
    echo "                   ctool instead."
    echo "---------------------------------------------------------------------"
    echo "Below is the dump of all offending lines:"
    echo " "
    cat ${OUTPUT_FILE}
    echo " "
    echo "---------------------------------------------------------------------"
    echo "Please fix these occurrences and run the script again."
}

BRANCH_NAME="$(git rev-parse --abbrev-ref HEAD)"

while [ $# -gt 0 ]; do
    case "$1" in
        --branch=*)
            if [ ! -z "${1#*=}" ]; then
                BRANCH_NAME="${1#*=}"
            fi
            ;;
        --excluded-dirs=*)
            IFS=',' read -ra EXTRA_DIRS_TO_EXCLUDE <<< "${1#*=}";
            EXCLUDED_DIRS+=("${EXTRA_DIRS_TO_EXCLUDE[@]}");
            ;;
        *)
            printf "***************************\n"
            printf "* Error: Invalid argument.*\n"
            printf "***************************\n"
            exit 1
    esac
    shift
done

VFS_TAG=`echo "${BRANCH_NAME}" | egrep -o 'VFS-[[:digit:]]+' | head -n1`
if [ "$BRANCH_NAME" == "develop" ]; then
    echo "Current branch is develop, the script will not look"
    echo "for forgotten todos marked with a specific VFS tag."
elif [ -z "${VFS_TAG}" ]; then
    echo "WARNING: Cannot resolve the VFS tag (e.g. VFS-1234). You should run this"
    echo "script in a git repo with a branch checked out that has such tag in its name."
    echo "You may also provide the branch name or the VFS tag in the first argument."
    echo "The script will NOT look for forgotten todos marked with a specific VFS tag."
    echo "---------------------------------------------------------------------"
    echo " "
else
    echo "Current branch tag: ${VFS_TAG}"
fi


EXCLUDE_GREP_OPTS=()
for DIR in "${EXCLUDED_DIRS[@]}"; do EXCLUDE_GREP_OPTS+=(--exclude-dir=${DIR}); done
for FILE in "${EXCLUDED_FILES[@]}"; do EXCLUDE_GREP_OPTS+=(--exclude=${FILE}); done

run_grep() {
    PATTERN=${1}
    FILEPATH=${2}
    if [ -d "${FILEPATH}" ]; then
        GREP_OPTS="-rIsin"
        # no postprocessing - just feed it further
        POST_PROCESS=( cat )
    else
        GREP_OPTS="-Isin"
        # add the file name as prefix to each line of the output for the same format as grep -r gives
        POST_PROCESS=( sed -e "s|^|${FILEPATH}:|" )
    fi
    grep "${EXCLUDE_GREP_OPTS[@]}" ${GREP_OPTS} ${PATTERN} ${FILEPATH} | grep -v "${IGNORE_LINE_TAG}" | "${POST_PROCESS[@]}"
}

check_path() {
    FILEPATH=${1}
    run_grep fixme ${FILEPATH} >> ${OUTPUT_FILE}
    run_grep 'writeme\s'  ${FILEPATH} >> ${OUTPUT_FILE}
    run_grep 'writeme$'  ${FILEPATH} >> ${OUTPUT_FILE}
    run_grep 'writeme:'  ${FILEPATH} >> ${OUTPUT_FILE}
    run_grep todo ${FILEPATH} | sed -E '/VFS-[0-9]+/d' >> ${OUTPUT_FILE}
    run_grep 'rpc:multicall' ${FILEPATH} >> ${OUTPUT_FILE}
    if [ -n "${VFS_TAG}" ]; then
        run_grep ${VFS_TAG} ${FILEPATH} >> ${OUTPUT_FILE}
    fi
}

# scan all the files and directories in CDW (internally skips EXCLUDED_FILES and EXCLUDED_DIRS)
find . -maxdepth 1 -mindepth 1 | while read FILEPATH;
do
    check_path ${FILEPATH};
done

# scan non-excluded deps in the lib directory (internally skips EXCLUDED_FILES and EXCLUDED_DIRS)
if [ -d "./_build/default/lib" ]; then
    find ./_build/default/lib -maxdepth 1 -mindepth 1 | while read FILEPATH
    do
        FILENAME=`basename ${FILEPATH}`
        if [[ ! " ${EXCLUDED_THIRD_PARTY_DEPS[@]} " =~ " ${FILENAME} " ]]; then
            check_path ${FILEPATH};
        fi
    done
else
    echo "Warning: could not find the '_build/default/lib' directory, skipping scan of dependencies"
fi

if [ -s ${OUTPUT_FILE} ]; then
    print_failure_summary
    exit 1
else
    echo "Success - no forgotten fixmes or todos found."
    exit 0
fi