# coding=utf-8
"""Author: Bartek Kryza
Copyright (C) 2020 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a HTTP server.
"""

import re
import subprocess
import sys
from timeouts import *

from . import common, docker


def _http_ready(container):
    try:
        settings = docker.inspect(container)
        host = settings['NetworkSettings']['IPAddress']
        output = docker.exec_(container,
                          ['curl', '-kSs', '--head',
                           '--user', 'user:password', 'https://{}/test_data/index.txt'.format(host)],
                          output=True,
                          stdout=sys.stderr)
    except subprocess.CalledProcessError:
        return False

    return bool(re.search('HTTP/1.1 200 OK', output))


def _node_up(image, name, uid):
    hostname = common.format_hostname([name, 'http'], uid)

    container = docker.run(
            image=image,
            hostname=hostname,
            name=hostname,
            privileged=True,
            detach=True)

    common.wait_until(_http_ready, [container], HTTP_READY_WAIT_SECONDS)

    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']

    return {
        'docker_ids': [container],
        'endpoint': "https://{}".format(ip),
        'credentials': 'user:password',
        'credentials_type': 'basic',
        'authorization_header': '',
        'verify_server_certificate': 'false',
        'connection_pool_size': '10'
    }


def up(image, name, uid):
    return _node_up(image, name, uid)
