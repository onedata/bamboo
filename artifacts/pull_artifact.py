#! /usr/bin/env python3
"""
Pulls build artifact from external repo.

Run the script with -h flag to learn about script's running options.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016-2018 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import argparse
from paramiko import SSHClient, AutoAddPolicy
from scp import SCPClient
import signal
import sys
from typing import Callable, Optional, Any, Tuple

from artifact_utils import artifact_path, ARTIFACTS_EXT, DEVELOP_BRANCH


def download_specific_or_default(ssh: SSHClient, plan: str, branch: str,
                                 hostname: str, port: int, username: str,
                                 default_branch: str = DEVELOP_BRANCH) -> None:
    """
    Downloads build artifact for specific plan and branch from repo.
    If artifact doesn't exist in repo, artifact from default (develop) branch
    is downloaded.
    :param ssh: sshclient with opened connection
    :param plan: name of current bamboo plan
    :param branch: name of current git branch
    :param hostname: hostname of artifacts repository
    :param port: SSH port
    :param username: username to authenticate as
    :param default_branch: name of default git branch
    """
    download_artifact_safe(
        ssh, plan, branch, hostname, port, username,
        exc_handler=download_default_artifact,
        exc_handler_args=(ssh, plan, default_branch, hostname, port,
                          username),
        exc_log="Artifact of plan {0}, specific for branch {1} not found, "
                "pulling artifact from branch {2}.".format(plan, branch,
                                                           default_branch))


def download_default_artifact(ssh: SSHClient, plan: str, branch: str,
                              hostname: str, port: int, username: str) -> None:
    """
    Downloads build artifact for specific plan from default branch.
    :param ssh: sshclient with opened connection
    :param plan: name of current bamboo plan
    :param branch: name of git branch
    :param hostname: hostname of artifacts repository
    :param port: SSH port
    :param username: username to authenticate as
    """
    download_artifact_safe(
        ssh, plan, branch, hostname, port, username,
        exc_log="Pulling artifact of plan {}, from branch {} failed."
                .format(plan, branch))


def download_artifact_safe(ssh: SSHClient, plan: str, branch: str,
                           hostname: str, port: int, username: str,
                           exc_handler: Optional[Callable[..., Any]] = None,
                           exc_handler_args: Tuple[Any, ...] = (),
                           exc_log: str = '') -> None:
    """
    Downloads artifact from repo. Locks file while it's being downloaded.
    If exception is thrown during download, exc_log is printed and
    exc_handler function is called.
    :param ssh: sshclient with opened connection
    :param plan: name of current bamboo plan
    :param branch: name of current git branch
    :param hostname: hostname of artifacts repository
    :param port: SSH port
    :param username: username to authenticate as
    :param exc_handler: function called when exception is thrown while
    artifact is being downloaded
    :param exc_handler_args: args for exc_handler
    :param exc_log: log that is printed when exception is thrown while
    artifact is being downloaded
    """

    def signal_handler(_signum, _frame):
        ssh.connect(hostname, port=port, username=username)
        sys.exit(1)

    signal.signal(signal.SIGINT, signal_handler)

    try:
        download_artifact(ssh, plan, branch)
    except:
        print(exc_log)
        if exc_handler:
            exc_handler(*exc_handler_args)


def download_artifact(ssh: SSHClient, plan: str, branch: str) -> None:
    """
    Downloads artifact from repo via SCP protocol.
    :param ssh: sshclient with opened connection
    :param plan: name of current bamboo plan
    :param branch: name of current git branch
    """
    with SCPClient(ssh.get_transport()) as scp:
        scp.get(artifact_path(plan, branch),
                local_path=plan.replace("-", '_') + ARTIFACTS_EXT)


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description='Push build artifacts.')

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
        '--branch', '-b',
        help='Name of current git branch',
        required=True)

    parser.add_argument(
        '--plan', '-pl',
        help='Name of current bamboo plan',
        required=True)

    args = parser.parse_args()

    ssh = SSHClient()
    ssh.set_missing_host_key_policy(AutoAddPolicy())
    ssh.load_system_host_keys()
    ssh.connect(args.hostname, port=args.port, username=args.username)

    download_specific_or_default(ssh, args.plan, args.branch, args.hostname,
                                 args.port, args.username)

    ssh.close()


if __name__ == '__main__':
    main()
