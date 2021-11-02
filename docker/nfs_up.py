#!/usr/bin/env python
# coding=utf-8

"""Authors: Tomasz Lichon
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script that brings up a nfs server.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

import argparse
import json

from environment import nfs, common, dockers_config

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up nfs server.')

parser.add_argument(
    '-i', '--image',
    action='store',
    default=None,
    help='override of docker image for the container',
    dest='image')

parser.add_argument(
    '-p', '--path',
    action='store',
    default=None,
    help='path on the host to be mounted as nfs share',
    dest='path')

args = parser.parse_args()
dockers_config.ensure_image(args, 'image', 'nfs')

config = nfs.up(args.image, common.generate_uid(), 'storage', args.path)

print(json.dumps(config))
