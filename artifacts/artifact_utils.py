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
import sys


ARTIFACTS_DIR = 'artifacts'
ARTIFACTS_EXT = '.tar.gz'


def build_local_path(file_path: str, artifact_name: str, plan: str):
    """
    Build the path on the client machine from which an artifact will be uploaded to the repo
    or to which an artifact will be downloaded from the repo.

    :file_path: path to the '+ARTIFACTS_EXT+' file to be pushed or pulled as an artifact.
    :artifact_name: name of the artifact in the repo.
    :plan: name of current bamboo plan.
    """
    
    if file_path:
        local_path = file_path
    elif artifact_name:
        local_path = artifact_name
    else:
        local_path = default_build_artifact_name(plan)
        print("Neither source file nor artifact name was specified, using default local path:", local_path)  
    return local_path


def build_repo_path(artifact_name: str, plan: str, branch: str) -> str:
    """
    The path in the artifacts repo to which an artifact is uploaded from the client
    or from which an artifact is downloaded to the client.

    :artifact_name: name of the artifact in the repo.
    :plan: name of current bamboo plan.
    :branch: name of current git branch.
    """
    if artifact_name:
        if not artifact_name.endswith(ARTIFACTS_EXT):
            print('The artifact name must have the extension \'{}\''.format(ARTIFACTS_EXT), file=sys.stderr)
            sys.exit(1)
        return os.path.join(ARTIFACTS_DIR, plan, branch, artifact_name)
    else:
        print("Artifact name was not specified, will be treated as a default build artifact")
        # the default build artifact name in the repo is an empty string
        return os.path.join(ARTIFACTS_DIR, plan, branch, default_build_artifact_name(plan))

    
def default_build_artifact_name(plan: str) -> str:
    """
    The default artifact is based on the plan in which dashes are replaced with underscores
    :plan: name of the plan
    """
    return plan.replace('-', '_') + ARTIFACTS_EXT
