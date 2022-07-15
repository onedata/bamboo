#! /usr/bin/env python3

"""
This file contains utility functions for scripts responsible for pushing
and pulling build artifacts.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import os
import time
import paramiko


ARTIFACTS_DIR = 'artifacts'
ARTIFACTS_EXT = '.tar.gz'
PARTIAL_EXT = '.partial'
DEVELOP_BRANCH = 'develop'


def named_artifact_path(plan: str, branch: str, artifact: str) -> str:
    """
    Returns path to artifact for specific plan and branch. Path is relative
    to user's home directory on repository machine.
    :param plan: name of current bamboo plan
    :param branch: name of current git branch
    """
    suffix = artifact.find(ARTIFACTS_EXT)
    artifact_base_name = artifact[:suffix]
    return os.path.join(ARTIFACTS_DIR, plan, branch, 
                        artifact_base_name +  ARTIFACTS_EXT)


def artifact_path(plan: str, branch: str) -> str:
    """
    Returns path to artifact for specific plan and branch. Path is relative
    to user's home directory on repository machine.
    :param plan: name of current bamboo plan
    :param branch: name of current git branch
    """
    return os.path.join(ARTIFACTS_DIR, plan, branch + ARTIFACTS_EXT)


def delete_file(ssh: paramiko.SSHClient, file_name: str) -> None:
    """
    Delete file named file_name via ssh.
    :param ssh: sshclient with opened connection
    :param file_name: name of file to be unlocked
    """

    ssh.exec_command("rm -rf {}".format(file_name))


def partial_extension() -> str:
    return "{partial}.{timestamp}".format(
        partial=PARTIAL_EXT,
        timestamp=time.time()
    )
