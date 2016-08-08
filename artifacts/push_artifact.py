#! /usr/bin/env python
"""
Pushes build artifact to external repo.
Artifact should be file with extension .tar.gz

Run the script with -h flag to learn about script's running options.
"""
import argparse
import os
from paramiko import SSHClient, AutoAddPolicy
from scp import SCPClient

ARTIFACTS_DIR = 'artifacts'
ARTIFACTS_EXT = '.tar.gz'


def upload_artifact(ssh, artifact, plan, branch):
    """
    Uploads given artifact to repo.
    :param ssh: sshclient with opened connection
    :type ssh: paramiko.SSHClient
    :param artifact: name of artifact to be pushed
    :type artifact: str
    :param plan: name of current bamboo plan
    :type plan: str
    :param branch: name of current git branch
    :type branch: str
    :return None
    """
    # TODO handle simultaneous pulling and pushing
    with SCPClient(ssh.get_transport()) as scp:
        scp.put(artifact, remote_path=os.path.join(ARTIFACTS_DIR, plan,
                                                   branch + ARTIFACTS_EXT))

parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Push build artifacts.')

parser.add_argument(
    '--hostname', '-hn',
    action='store',
    help='Hostname of artifacts repository',
    dest='hostname',
    required=True)

parser.add_argument(
    '--port', '-p',
    action='store',
    type=int,
    help='SSH port to connect to',
    dest='port',
    required=True)

parser.add_argument(
    '--username', '-u',
    action='store',
    help='The username to authenticate as',
    dest='username',
    required=True)

parser.add_argument(
    '--artifact', '-a',
    action='store',
    help='Artifact to be pushed. It should be file with .tar.gz extension',
    dest='artifact',
    required=True)

parser.add_argument(
    '--branch', '-b',
    action='store',
    help='Name of current git branch',
    dest='branch',
    required=True)

parser.add_argument(
    '--plan', '-pl',
    action='store',
    help='Name of current bamboo plan',
    dest='plan',
    required=True)

args = parser.parse_args()

ssh = SSHClient()
ssh.set_missing_host_key_policy(AutoAddPolicy())
ssh.load_system_host_keys()
ssh.connect(args.hostname, port=args.port, username=args.username)

try:
    upload_artifact(ssh, args.artifact, args.plan, args.branch)
except:
    print "Uploading artifact of plan {0}, on branch {1} failed"\
        .format(args.plan, args.branch)

ssh.close()
