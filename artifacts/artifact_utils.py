#! /usr/bin/env python3

"""
This file contains utility functions for scripts responsible for pushing
and pulling build artifacts.
"""
__author__ = "Jakub Kudzia, Darin Nikolow"
__copyright__ = "Copyright (C) 2016-2022 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import os
import time
import paramiko


ARTIFACTS_DIR = 'artifacts'
ARTIFACTS_EXT = '.tar.gz'
PARTIAL_EXT = '.partial'
DEVELOP_BRANCH = 'develop'


def choose_artifact_paths(artifact_name: str, source_file: str, plan: str, branch: str):
    if (artifact_name != 'None'):
        if (source_file == 'None'):
            src_path = artifact_name
        else:
            src_path = source_file
        dst_path = named_artifact_path(plan, branch, artifact_name)
    else:
        if (source_file == 'None'):
            src_path = plan.replace("-", '_') + ARTIFACTS_EXT
            print("Arifact name was not specified, using default name: ", src_path)
        else:
            src_path = source_file
        dst_path = artifact_path(plan, branch)
    return src_path, dst_path

def named_artifact_path(plan: str, branch: str, artifact: str) -> str:
    """
    Returns path to artifact for specific plan and branch. Path is relative
    to user's home directory on repository machine.
    :param plan: name of current bamboo plan
    :param branch: name of current git branch
    :param artifact: artifact name
    """
    # suffix = artifact.find(ARTIFACTS_EXT)
    # artifact_base_name = artifact[:suffix]
    return os.path.join(ARTIFACTS_DIR, plan, branch, artifact)


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
