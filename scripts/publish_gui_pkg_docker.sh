#!/bin/bash

# Author: Jakub Liput
# Copyright (C) 2019 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# Invoke this from main project repository dir.
# Parameters of this script are passed to ./docker_build.py script.
#
# For complete publish you should add:
# --user <onedata_repo_username>
# --password <onedata_repo_password>
#
# Example invocation on Bamboo:
# ./bamboos/scripts/gui/publish_gui_pkg_docker.sh --user bamboo --password <some_pass>

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

rm -rf gui_static gui_static.tar.gz
cp -r rel gui_static || exit $?
tar -zcf gui_static.tar.gz gui_static || exit $?
rm -rf gui_static

PKG_SHA_SUM=`shasum -a 256 gui_static.tar.gz | cut -f1 -d ' '`
echo "Package SHA-256: ${PKG_SHA_SUM}"

${SCRIPT_DIR}/../docker/docker_build.py --repository docker.onedata.org --tag "SHA256-${PKG_SHA_SUM}" --name $(basename "$PWD") --publish --remove $@ .

rm -f gui_static.tar.gz
