#! /usr/bin/env python

import argparse
import os
from paramiko import SSHClient, AutoAddPolicy
from scp import SCPClient

ARTIFACTS_DIR = 'artifacts'
ARTIFACTS_EXT = '.tar.gz'

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Push build artifacts.')

parser.add_argument(
    '--hostname', '-hn',
    action='store',
    help='Hostname of artifacts repository',
    dest='hostname')

parser.add_argument(
    '--port', '-p',
    action='store',
    type=int,
    help='SSH port to connect to',
    dest='port')

parser.add_argument(
    '--username', '-u',
    action='store',
    help='The username to authenticate as',
    dest='username')

parser.add_argument(
    '--artifact', '-a',
    action='store',
    help='Artifact to push',
    dest='artifact')

parser.add_argument(
    '--branch', '-b',
    action='store',
    help='Name of a branch',
    dest='branch')

parser.add_argument(
    '--plan', '-pl',
    action='store',
    help='Name of plan',
    dest='plan')

ssh = SSHClient()
ssh.set_missing_host_key_policy(AutoAddPolicy())
ssh.load_system_host_keys()

[args, pass_args] = parser.parse_known_args()

ssh.connect(args.hostname, port=args.port, username=args.username)

scp = SCPClient(ssh.get_transport())
scp.put(args.artifact, remote_path=os.path.join(ARTIFACTS_DIR, args.plan,
                                                args.branch + ARTIFACTS_EXT))
