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


# the path on the client machine from which an artifact is uploaded to the repo
# or to which an artifact is downloaded from the repo
def build_local_path(source_file: str, artifact_name: str, plan: str):
    if artifact_name == 'None':
        if source_file == 'None':
            local_path = plan.replace("-", '_') + ARTIFACTS_EXT
            print("Source path was not specified, using default local path: ", local_path)
        else:
            local_path = source_file
    else:
        if source_file == 'None':
            local_path = artifact_name
        else:
            local_path = source_file
    return local_path


# the path in the artifacts repo to which an artifact is uploaded from the client
# or from which an artifact is downloaded to the client
def build_repo_path(artifact_name: str, plan: str, branch: str) -> str:
    if artifact_name:
        if not artifact_name.endswith(ARTIFACTS_EXT):
            print('The artifact name must have the extension \'{}.\''.format(ARTIFACTS_EXT), file=sys.stderr)
            sys.exit(1)
        return os.path.join(ARTIFACTS_DIR, plan, branch, artifact_name)
    else:
        print("Artifact name was not specified, will use default build artifact name")
        # the default build artifact name in the repo is an empty string
        return os.path.join(ARTIFACTS_DIR, plan, branch + ARTIFACTS_EXT)
