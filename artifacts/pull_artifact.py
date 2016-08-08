#! /usr/bin/env python
"""
Pulls build artifact from external repo.

Run the script with -h flag to learn about script's running options.
"""
import argparse
import os
from paramiko import SSHClient, AutoAddPolicy
from scp import SCPClient, SCPException

ARTIFACTS_DIR = 'artifacts'
ARTIFACTS_EXT = '.tar.gz'
DEFAULT_BRANCH = 'develop'


def download_artifact(ssh, plan, branch=DEFAULT_BRANCH):
    """
    Downloads artifact from repo. If branch  argument is not passed, downloads
    artifact from DEFAULT_BRANCH
    :param ssh: sshclient with opened connection
    :type ssh: paramiko.SSHClient
    :param plan: name of current bamboo plan
    :type plan: str
    :param branch: name of current git branch
    :type branch: str
    :return None
    """
    with SCPClient(ssh.get_transport()) as scp:
        scp.get(os.path.join(ARTIFACTS_DIR, plan, branch + ARTIFACTS_EXT),
                local_path=args.plan.replace("-", '_') + ARTIFACTS_EXT)


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
    download_artifact(ssh, args.plan, args.branch)
except SCPException:
    print ("Artifact of plan {0}, specific for branch {1} not found, "
           "pulling artifact from branch develop.").format(args.plan, args.branch)
    try:
        download_artifact(ssh, args.plan)
    except:
        print "Pulling artifact of plan {}, from branch develop failed."\
            .format(args.plan)

ssh.close()
