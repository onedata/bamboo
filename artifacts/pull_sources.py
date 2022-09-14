#! /usr/bin/env python3
"""
Pulls artifacts from external repo using branches defined in branchConfig.yaml
file.

Run the script with -h flag to learn about script's running options.
"""
__author__ = "Michal Cwiertnia"
__copyright__ = "Copyright (C) 2018 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import os
import yaml
import argparse
import boto3
from paramiko import SSHClient, AutoAddPolicy

from pull_artifact import (download_artifact_safe,
                           download_specific_or_default,
                           s3_download_artifact_safe,
                           s3_download_specific_or_default)


BRANCH_CFG_PATH = 'branchConfig.yaml'
BAMBOO_BRANCH_NAME = 'bamboo_planRepository_branchName'
DEFAULT_BRANCH = 'default'
CURRENT_BRANCH = 'current_branch'


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description='Pull sources and images lists for branches specified in '
                    'branchConfig.yaml file.')

    parser.add_argument(
        '--hostname', '-hn',
        help='Hostname of artifacts repository',
        required=True)

    parser.add_argument(
        '--port', '-p',
        type=int,
        help='SSH port to connect to',
        required=True)

    parser.add_argument(
        '--username', '-u',
        help='The username to authenticate as',
        required=True)

    parser.add_argument(
        '--s3-url',
        help='The S3 endpoint URL',
        default='https://storage.cloud.cyfronet.pl')

    parser.add_argument(
        '--s3-bucket',
        help='The S3 bucket name',
        default='bamboo-artifacts-2')

    args = parser.parse_args()

    if args.hostname != 'S3':
        ssh = SSHClient()
        ssh.set_missing_host_key_policy(AutoAddPolicy())
        ssh.load_system_host_keys()
        ssh.connect(args.hostname, port=args.port, username=args.username)

        with open(BRANCH_CFG_PATH, 'r') as branch_cfg_file:
            branch_cfg = yaml.load(branch_cfg_file)
            default_branch = branch_cfg.get(DEFAULT_BRANCH)

            for plan, branch in branch_cfg.get('sources').items():
                if branch != CURRENT_BRANCH:
                    print('Getting artifact for plan {}\'s from branch {}'
                          .format(plan, branch))
                    exc_log = 'Branch {} in plan {} not found.'.format(branch,
                                                                   plan)
                    download_artifact_safe(ssh, plan, branch, args.hostname,
                                           args.port, args.username,
                                           exc_handler=exit,
                                           exc_handler_args=(1,),
                                           exc_log=exc_log)
                else:
                    download_specific_or_default(ssh, plan,
                                                 os.getenv(BAMBOO_BRANCH_NAME),
                                                 args.hostname, args.port,
                                                 args.username,
                                                 default_branch=default_branch)
        ssh.close()
    else:
        s3_session = boto3.session.Session()
        s3_res = s3_session.resource(
            service_name='s3',
            endpoint_url=args.s3_url
        )

        with open(BRANCH_CFG_PATH, 'r') as branch_cfg_file:
            branch_cfg = yaml.load(branch_cfg_file)
            default_branch = branch_cfg.get(DEFAULT_BRANCH)

            for plan, branch in branch_cfg.get('sources').items():
                if branch != CURRENT_BRANCH:
                    print('Getting artifact for plan {}\'s from branch {}'
                          .format(plan, branch))
                    exc_log = 'Branch {} in plan {} not found.'.format(branch,
                                                                       plan)
                    s3_download_artifact_safe(s3_res, args.s3_bucket, plan, branch, args.hostname,
                                              args.port, args.username,
                                              exc_handler=exit,
                                              exc_handler_args=(1,),
                                              exc_log=exc_log)
                else:
                    s3_download_specific_or_default(s3_res, args.s3_bucket, plan,
                                                    os.getenv(BAMBOO_BRANCH_NAME),
                                                    args.hostname, args.port,
                                                    args.username,
                                                    default_branch=default_branch)


if __name__ == '__main__':
    main()
