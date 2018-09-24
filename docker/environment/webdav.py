# coding=utf-8
"""Author: Bartek Kryza
Copyright (C) 2018 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a WebDAV server.
"""

import re
import subprocess
import sys
from timeouts import *

from . import common, docker


def _webdav_ready(container):
    try:
        output = docker.exec_(container,
                          ['curl', '-s', '-X', 'OPTIONS', '--head',
                           '-u', 'admin:password', 'http://localhost:80'],
                          output=True,
                          stdout=sys.stderr)
    except subprocess.CalledProcessError:
        return False

    return bool(re.search('HTTP/1.1 200 OK', output))


def _node_up(image, name, uid):
    hostname = common.format_hostname([name, 'webdav'], uid)

    container = docker.run(
            image=image,
            hostname=hostname,
            name=hostname,
            privileged=True,
            detach=True)

    common.wait_until(_webdav_ready, [container], WEBDAV_READY_WAIT_SECONDS)

    credentials = 'admin:password'
    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']

    return {
        'docker_ids': [container],
        'credentials': credentials,
        'host_name': ip
    }


def up(image, name, uid):
    return _node_up(image, name, uid)
