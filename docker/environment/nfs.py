# coding=utf-8
"""Author: Tomasz Lichon
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a nfs server.
"""

import re
import sys

from . import docker
from . import common
from timeouts import NFS_READY_WAIT_SECONDS

def _nfs_ready(container):
    output = docker.exec_(container, ['bash', '-c', 'ps aux | grep rpc | grep -v grep'],
                          output=True, stdout=sys.stderr)
    return bool(re.search('rpcbind', output))


def _node_up(image, uid, name, path):
    hostname = common.format_hostname([name.replace('/', '-').strip('-'), 'nfs'], uid)
    container = docker.run(
        image=image,
        detach=True,
        name=hostname,
        hostname=hostname,
        volumes=(path, "/nfsshare", "rw"),
        privileged=True)

    common.wait_until(_nfs_ready, [container], NFS_READY_WAIT_SECONDS)

    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']

    return {
        'docker_ids': [container],
        'host': ip,
    }


def up(image, uid, name, path):
    return _node_up(image, uid, name, path)
