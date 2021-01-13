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
REPORT_FILENAME=gui-image.conf
PROJECT_NAME=$(basename "$PWD")

rm -rf gui_static gui_static.tar.gz
cp -r rel gui_static || exit $?
tar -zcf gui_static.tar.gz gui_static || exit $?
rm -rf gui_static

COMMIT=`git rev-parse HEAD | cut -c1-10`
PKG_SHA_SUM=`shasum -a 256 gui_static.tar.gz | cut -f1 -d ' '`
PKG_SHA_SUM_PREFIX=`echo ${PKG_SHA_SUM} | cut -c1-10`
echo "Package SHA-256: ${PKG_SHA_SUM}"

SHA256_TAG="SHA256-${PKG_SHA_SUM}"
ID_SHA256P_TAG="ID_SHA256P-${COMMIT}-${PKG_SHA_SUM_PREFIX}"

${SCRIPT_DIR}/../docker/docker_build.py \
  --name ${PROJECT_NAME} \
  --repository docker.onedata.org \
  --tag ${SHA256_TAG} \
  --tag ${ID_SHA256P_TAG} \
  --publish \
  --remove \
  $@ .

cat > ${REPORT_FILENAME} <<EOF
# ------------------------------------------------------------------------------
# This is a configuration file for the pull-gui.sh script, for more see:
#   _build/default/lib/gui/pull-gui.sh
#
# The pull-gui.sh script fetches a GUI package (static files) and places it in
# deps directory (see pull-gui.sh -> TARGET_PATH). The package must be moved to
# the target location during release generation (usually done in rebar.config).
# The script attempts to download specified docker image from two repositories:
# docker.onedata.org (primary) or dockerhub (fallback).
# ------------------------------------------------------------------------------

# The name of the docker image containing the GUI package.
IMAGE_NAME="${PROJECT_NAME}"
# Tag (version) of the above image.
IMAGE_TAG="${ID_SHA256P_TAG}"
# SHA-256 checksum of the GUI package (tar.gz archive) embedded in the docker.
# Used for additional integrity check and to keep track of verified GUI packages.
PACKAGE_CHECKSUM="${PKG_SHA_SUM}"
EOF

rm -f gui_static.tar.gz
