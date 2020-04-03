# coding=utf-8
"""Authors: Jakub Kudzia
Copyright (C) 2020 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script that brings up a python LUMA.
"""
import ConfigParser
import json
import StringIO

from . import docker, common

LUMA_PORT = 8080
LUMA_PATH = "/api/v3/luma"


def _node_up(image, db_path, uid):
    hostname = common.format_hostname('luma', uid)

    volumes = []
    if db_path:
        volumes = [(db_path, '/luma/db.json', 'rw')]

    container = docker.run(
        image=image,
        detach=True,
        volumes=volumes,
        privileged=True,
        output=True,
        name=hostname,
        hostname=hostname)

    settings = docker.inspect(container)
    ip = settings['NetworkSettings']['IPAddress']

    return {'docker_id': container, 'url': "http://{0}:{1}{2}".format(ip, LUMA_PORT, LUMA_PATH)}


def up(image, db_path, uid):
    return _node_up(image, db_path, uid)
