#! /usr/bin/env python3
"""
Pushes build or arbitrary artifact to external repo. Artifacts are 
file packages - typically gzip compressed tarballs. Build artifacts 
in the remote external repo are always named based on the plan repo and 
the plan branch. For example, `op-worker/develop.tar.gz`. The name 
of the local build artifact is always based on the plan name. For 
example `op_worker.tar.gz`. Note that `-` is replaced by `_`.  
Arbitrary artifacts may have arbitrary names. In this case the local 
and remote artifact names are identical. The remote artifact is placed
in a directory according to the plan and branch, For example, 
op-worker/feature/VFS-1234-my-branch/. Locally the artifact is placed 
in the current dir by default but can be specified with --source-file.

Run the script with -h flag to learn about script's running options.
"""
__author__ = "Jakub Kudzia, Darin Nikolow"
__copyright__ = "Copyright (C) 2016-2022 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import signal
import sys
import argparse
from paramiko import SSHClient, AutoAddPolicy, SSHException
from scp import SCPClient, SCPException
from artifact_utils import (choose_artifact_paths, named_artifact_path, artifact_path, delete_file,
                            partial_extension, ARTIFACTS_EXT)
import boto3

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
        '--artifact', '-a',
        help='IGNORED, use --artifact-name instead. Artifact to be pushed. It should be file with .tar.gz extension',
        default='None',
        required=False)

    parser.add_argument(
        '--artifact-name', '-an',
        help='Artifact to be pushed. It should be file with .tar.gz extension',
        default='None',
        required=False)

    parser.add_argument(
        '--source-file', '-sf',
        help='Path to a file to read the artifact',
        default='None',
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

def upload_artifact_safe(ssh: SSHClient, artifact: str, plan: str,
                         branch: str, hostname: str, port: int,
                         username: str, artifact_name: str, source_file: str) -> None:
    src_path, dst_path = choose_artifact_paths(artifact_name, source_file, plan, branch)
    print(src_path, dst_path)
    ext = partial_extension()
    partial_file_name = dst_path + ext

    def signal_handler(_signum, _frame):
        ssh.connect(hostname, port=port, username=username)
        delete_file(ssh, partial_file_name)
        sys.exit(1)
    signal.signal(signal.SIGINT, signal_handler)

    try:
        upload_artifact(ssh, src_path, partial_file_name)
        rename_uploaded_file(ssh, partial_file_name, dst_path)
    except (SCPException, SSHException) as e:
        print("Uploading artifact of plan {0}, on branch {1} failed"
              .format(plan, branch))
        delete_file(ssh, partial_file_name)
        raise e

def s3_upload_artifact_safe(s3: boto3.resources, bucket: str, artifact: str, plan: str,
                            branch: str, artifact_name: str, source_file) -> None:
    src_path, dst_path = choose_artifact_paths(artifact_name, source_file, plan, branch)
    print(src_path, dst_path)
    data = open(src_path, 'rb')
    buck = s3.Bucket(bucket)
    buck.put_object(Key=dst_path, Body=data)


def upload_artifact(ssh: SSHClient, artifact: str, remote_path: str) -> None:
    """
    Uploads given artifact to repo.
    :param ssh: sshclient with opened connection
    :param artifact: name of artifact to be pushed
    :param remote_path: path for uploaded file
    """
    with SCPClient(ssh.get_transport()) as scp:
        scp.put(artifact, remote_path=remote_path)

        
def rename_uploaded_file(ssh: SSHClient, src_file: str,
                         target_file: str) -> None:
    ssh.exec_command("mv {0} {1}".format(src_file, target_file))


def main():
    args = parse_args()
    if args.artifact != 'None':
        print('The option --artifact is ignored. The name of artifact is ', args.plan.replace('-', '_') + ARTIFACTS_EXT, '. To specify another name use --artifact_name.', sep="") 
    if args.hostname != 'S3':
        ssh = SSHClient()
        ssh.set_missing_host_key_policy(AutoAddPolicy())
        ssh.load_system_host_keys()
        ssh.connect(args.hostname, port=args.port, username=args.username)
        upload_artifact_safe(ssh, args.artifact, args.plan, args.branch,
                             args.hostname, args.port, args.username,
                             args.artifact_name, args.source_file)
        ssh.close()
    else:
        s3_session = boto3.session.Session()

        s3_res = s3_session.resource(
            service_name='s3',
            endpoint_url=args.s3_url
        )
        s3_upload_artifact_safe(s3_res, args.s3_bucket, args.artifact, args.plan,
                                args.branch, args.artifact_name, args.source_file)
        

if __name__ == '__main__':
    main()
