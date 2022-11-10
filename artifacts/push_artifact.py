#! /usr/bin/env python3
"""
Pushes an artifact to external repo. Artifacts are file packages -
typically gzip compressed tarballs.

Artifacts are identified by names, which are later used during pulling.
If no name is provided, default build artifact name is used.

Build artifacts in the external repo are always named based on the
plan's repo and branch, for example: `op-worker/develop/op_worker.tar.gz`.
The local name of the build archive is always based on the plan name.
For example: `op_worker.tar.gz` (note that dashes are replaced by underscores).

Artifacts with custom names are placed in a directory depending on
the plan's repo and branch, for example:
`op-worker/feature/VFS-1234-my-branch/custom-artifact.tar.gz`

Run the script with -h flag to learn about script's running options.
"""
__author__ = "Jakub Kudzia, Darin Nikolow"
__copyright__ = "Copyright (C) 2016-2022 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import signal
import sys
import argparse
import time
import boto3
import artifact_utils

from paramiko import SSHClient, AutoAddPolicy, SSHException
from scp import SCPClient, SCPException
from artifact_utils import *

PARTIAL_EXT = '.partial'

def parse_args():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description='Push build or arbitratry artifacts.')

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
        '--artifact-name', '-an',
        help='Name for the artifact, with '+ARTIFACTS_EXT+' extension, used for artifact identification. ' +
             'If not specified, uses default build artifact name.',
        default=None,
        required=False)

    parser.add_argument(
        '--source-file-path', '-sf',
        help='Path to the '+ARTIFACTS_EXT+' file to be pushed as an artifact. Defaults to the artifact name in CWD.',
        default=None,
        required=False)

    parser.add_argument(
        '--branch', '-b',
        help='Name of current git branch',
        required=True)

    parser.add_argument(
        '--plan', '-pl',
        help='Name of current bamboo plan',
        required=True)

    parser.add_argument(
        '--s3-url',
        help='The S3 endpoint URL',
        default='https://storage.cloud.cyfronet.pl')

    parser.add_argument(
        '--s3-bucket',
        help='The S3 bucket name',
        default='bamboo-artifacts-2')

    return parser.parse_args()

def ssh_upload_artifact(ssh: SSHClient, plan: str, branch: str, hostname: str, port: int,
                         username: str, artifact_name: str, source_file_path: str) -> None:
    src_path = artifact_utils.build_local_path(source_file_path, artifact_name, plan)
    dst_path = artifact_utils.build_repo_path(artifact_name, plan, branch)
    print("Uploading artifact")
    print("    source path: {}".format(src_path))
    print("    dest.  path: {}".format(dst_path))

    partial_file_name = dst_path + partial_extension()

    def signal_handler(_signum, _frame):
        ssh.connect(hostname, port=port, username=username)
        delete_file(ssh, partial_file_name)
        sys.exit(1)

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    try:
        ssh_upload_artifact_unsafe(ssh, src_path, partial_file_name)
        rename_file(ssh, partial_file_name, dst_path)
    except (SCPException, SSHException) as e:
        print("Uploading artifact of plan {0}, on branch {1} failed"
              .format(plan, branch))
        delete_file(ssh, partial_file_name)
        raise e

def ssh_upload_artifact_unsafe(ssh: SSHClient, artifact_name: str, remote_path: str) -> None:
    """
    Uploads given artifact to repo.
    :param ssh: sshclient with opened connection
    :param artifact_name: name of artifact to be pushed
    :param remote_path: path for uploaded file
    """
    with SCPClient(ssh.get_transport()) as scp:
        scp.put(artifact_name, remote_path=remote_path)

def partial_extension() -> str:
    return "{partial}.{timestamp}".format(
        partial=PARTIAL_EXT,
        timestamp=time.time()
    )

def rename_file(ssh: SSHClient, src_file: str,
                target_file: str) -> None:
    ssh.exec_command("mv {0} {1}".format(src_file, target_file))

def delete_file(ssh: SSHClient, file_name: str) -> None:
    """
    Delete file named file_name via ssh.
    :param ssh: sshclient with opened connection
    :param file_name: name of file to be unlocked
    """
    ssh.exec_command("rm -rf {}".format(file_name))

def s3_upload_artifact(s3: boto3.resources, bucket: str, plan: str,
                            branch: str, artifact_name: str, source_file_path) -> None:
    src_path = artifact_utils.build_local_path(source_file_path, artifact_name, plan)
    dst_path = artifact_utils.build_repo_path(artifact_name, plan, branch)
    print("Uploading artifact")
    print("    source path: {}".format(src_path))
    print("    dest.  path: {}".format(dst_path))

    data = open(src_path, 'rb')
    buck = s3.Bucket(bucket)
    buck.put_object(Key=dst_path, Body=data)

def main():
    args = parse_args()
    if args.hostname != 'S3':
        ssh = SSHClient()
        ssh.set_missing_host_key_policy(AutoAddPolicy())
        ssh.load_system_host_keys()
        ssh.connect(args.hostname, port=args.port, username=args.username)
        ssh_upload_artifact(ssh, args.plan, args.branch,
                             args.hostname, args.port, args.username,
                             args.artifact_name, args.source_file_path)
        ssh.close()
    else:
        s3_session = boto3.session.Session()

        s3_res = s3_session.resource(
            service_name='s3',
            endpoint_url=args.s3_url
        )
        s3_upload_artifact(s3_res, args.s3_bucket, args.plan,
                                args.branch, args.artifact_name, args.source_file_path)


if __name__ == '__main__':
    main()
