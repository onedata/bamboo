#!/bin/bash
# Author: Jakub Liput
# Copyright (C) 2020 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# Generate static html file with ReDoc documentation or start a server that renders
# API docs using provided swagger.json file.
# * no script parameter - generate a standalone HTML page with Redoc API browser.
#   Generated page will use standalone redoc JS linked to `/assets/redoc.standalone.js`
#   so it must be provided for serving website.
# * `preview` - start a server on http://localhost:8088 that will render ReDoc
#   documentation based on `swagger.json` file placed in working dir.

# Currently used version of Redoc standalone is: 2.0.0-rc.23

REDOC_IMG='onedata/redoc-cli:v3'

test -t 1 && TTY="-t"

if [ "$(uname)" == "Darwin" ]; then
    OPEN_CMD="open"
else
    OPEN_CMD="xdg-open"
fi

case "${1}" in
    preview)
        [ ! -f `pwd`/swagger.json ] && echo "No swagger.json file found in working directory" && exit 1
        echo "Serving on http://localhost:8088 (or http://\${DOCKER_MACHINE_IP}:8088)"
        echo "(should open automatically in your browser)"
        ASYNC_CMD=$(cat <<EOF
timeout 10m bash -c "until curl http://localhost:8088 &>/dev/null; do sleep 0.2; done && ${OPEN_CMD} http://localhost:8088" && sleep 1 && echo "Press Ctrl+C to kill the docker container and exit the preview."
EOF
)
        bash -c "${ASYNC_CMD}" &
        docker run --rm -v `pwd`/swagger.json:/var/www/html/swagger.json:ro -p 8088:80 ${REDOC_IMG}
        ;;

    *)
        docker run --rm -i ${TTY} -v `pwd`:/docs:delegated ${REDOC_IMG} sh -c "\
            cd /docs;\
            redoc-cli bundle --cdn -o redoc-static.html swagger.json &&\
            sed -i 's,https://unpkg.com/redoc@next/bundles/redoc.standalone.js,/assets/redoc.standalone.js,g' redoc-static.html" ||\
            exit $?
        ;;
esac

