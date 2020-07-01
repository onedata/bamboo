# coding=utf-8
"""Author: Bartek Kryza
Copyright (C) 2020 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a XRootD server.
"""

import re
import subprocess
import sys
from timeouts import *

from . import common, docker


def _xrootd_ready(container):
    try:
        settings = docker.inspect(container)
        host = settings['NetworkSettings']['IPAddress']
        output = docker.exec_(container,
                ['xrdfs', 'root://{}/'.format(host), 'stat', '/data'],
                          output=True,
                          stdout=sys.stderr)
    except subprocess.CalledProcessError:
        return False

    return bool(re.search('Path:\s+/data', output))


def _node_up(image, name, uid):
    hostname = common.format_hostname([name, 'xrootd'], uid)

    container = docker.run(
            image=image,
            hostname=hostname,
            name=hostname,
            privileged=True,
            detach=True)

    common.wait_until(_xrootd_ready, [container], XROOTD_READY_WAIT_SECONDS)

    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']

    return {
        'docker_ids': [container],
        'url': "root://{}/data".format(ip),
        'credentials_type': 'none',
        'credentials': ''
    }


def up(image, name, uid):
    return _node_up(image, name, uid)
