#!/bin/bash
# Author: Jakub Liput
# Copyright (C) 2020 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# Using swagger.json file, generate a standalone HTML page with Redoc API browser.
# Generated page will use standalone redoc JS linked to `/assets/redoc.standalone.js`
# so it must be provided for serving website.

# Currently used version of Redoc standalone is: 2.0.0-rc.23

REDOC_IMG='onedata/redoc-cli:v2'

test -t 1 && TTY="-t"

docker run --rm -i ${TTY} -v `pwd`:/docs:delegated ${REDOC_IMG} sh -c "\
  cd /docs;\
  redoc-cli bundle --cdn -o redoc-static.html swagger.json &&\
  sed -i 's,https://unpkg.com/redoc@next/bundles/redoc.standalone.js,/assets/redoc.standalone.js,g' redoc-static.html" ||\
  exit $?

