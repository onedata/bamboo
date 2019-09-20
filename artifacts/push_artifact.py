#! /usr/bin/env python3
"""
Pushes build artifact to external repo.
Artifact should be file with extension .tar.gz

Run the script with -h flag to learn about script's running options.
"""
__author__ = "Jakub Kudzia"
__copyright__ = "Copyright (C) 2016-2018 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import signal
import sys
import argparse
from paramiko import SSHClient, AutoAddPolicy, SSHException
from scp import SCPClient, SCPException
from artifact_utils import (artifact_path, delete_file, partial_extension)
import boto3

def upload_artifact_safe(ssh: SSHClient, artifact: str, plan: str,
                         branch: str, hostname: str, port: int,
                         username: str) -> None:

    file_name = artifact_path(plan, branch)
    ext = partial_extension()
    partial_file_name = file_name + ext

    def signal_handler(_signum, _frame):
        ssh.connect(hostname, port=port, username=username)
        delete_file(ssh, partial_file_name)
        sys.exit(1)
    signal.signal(signal.SIGINT, signal_handler)

    try:
        upload_artifact(ssh, artifact, partial_file_name)
        rename_uploaded_file(ssh, partial_file_name, file_name)
    except (SCPException, SSHException) as e:
        print("Uploading artifact of plan {0}, on branch {1} failed"
              .format(plan, branch))
        delete_file(ssh, partial_file_name)
        raise e


def s3_upload_artifact_safe(s3, bucket: str, artifact: str, plan: str,
                         branch: str) -> None:

    file_name = artifact_path(plan, branch)
    ext = partial_extension()
    partial_file_name = file_name + ext
    #print(file_name)
    data = open(artifact, 'rb')
    buck = s3.Bucket(bucket)
    buck.put_object(Key=file_name, Body=data)


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
        '--artifact', '-a',
        help='Artifact to be pushed. It should be file with .tar.gz extension',
        required=True)

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
        default='https://storage.cloud.cyfronet.pl',
        required=False)

    parser.add_argument(
        '--s3-bucket',
        help='The S3 bucket name',
        default='bamboo-artifacts-2',
        required=False)

    args = parser.parse_args()

    if args.hostname != 'S3':
        ssh = SSHClient()
        ssh.set_missing_host_key_policy(AutoAddPolicy())
        ssh.load_system_host_keys()
        ssh.connect(args.hostname, port=args.port, username=args.username)
        upload_artifact_safe(ssh, args.artifact, args.plan, args.branch,
                             args.hostname, args.port, args.username)
        ssh.close()
    else:
        s3_session = boto3.session.Session()

        #s3_client = s3_session.client(
        s3_res = s3_session.resource(
            service_name='s3',
            endpoint_url=args.s3_url
        )
        s3_upload_artifact_safe(s3_res, args.s3_bucket, args.artifact, args.plan,
                                args.branch)
        

if __name__ == '__main__':
    main()
