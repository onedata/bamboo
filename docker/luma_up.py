#!/usr/bin/env python
# coding=utf-8

"""Authors: Michal Wrona
Authors: Jakub Kudzia
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

A script that brings up a LUMA REST server.
More info about the server can be found here: https://git.onedata.org/projects/VFS/repos/luma
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function
import argparse
import json

from environment import luma, common, dockers_config

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Bring up LUMA python.')

parser.add_argument(
    '-i', '--image',
    action='store',
    default=None,
    help='override of docker image for the container',
    dest='image')

parser.add_argument(
    '-db', '--db_path',
    action='store',
    default=None,
    help='path to json DB file',
    dest='db_path')

parser.add_argument(
    '-u', '--uid',
    action='store',
    default=common.generate_uid(),
    help='uid that will be concatenated to docker names',
    dest='uid')

args = parser.parse_args()
dockers_config.ensure_image(args, 'image', 'luma')

config = luma.up(args.image, args.db_path, args.uid)

print(json.dumps(config))
