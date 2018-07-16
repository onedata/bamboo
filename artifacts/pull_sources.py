import os
import yaml
import argparse
from paramiko import SSHClient, AutoAddPolicy

from pull_artifact import (download_artifact_safe,
                           download_specific_or_develop)


BRANCH_CFG_PATH = 'branchConfig.yaml'
BAMBOO_BRANCH_NAME = 'bamboo_planRepository_branchName'
DEFAULT_BRANCH = 'default'


parser = argparse.ArgumentParser(
    formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    description='Pull sources and images lists for branches specified in '
                'branchConfig.yaml file.')

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


def main():
    args = parser.parse_args()

    ssh = SSHClient()
    ssh.set_missing_host_key_policy(AutoAddPolicy())
    ssh.load_system_host_keys()
    ssh.connect(args.hostname, port=args.port, username=args.username)

    with open(BRANCH_CFG_PATH, 'r') as branch_cfg_file:
        branch_cfg = yaml.load(branch_cfg_file)
        default_branch = branch_cfg.get(DEFAULT_BRANCH)

        for plan, branch in branch_cfg.get('branches').items():
            if branch != DEFAULT_BRANCH or default_branch != 'develop':
                if branch == DEFAULT_BRANCH:
                    branch = default_branch
                print('Getting artifact for plan {}\'s from branch {}'.
                      format(plan, branch))
                exception_log = 'Branch {} in plan {} not found.'.format(
                    branch, plan)
                download_artifact_safe(ssh, plan, branch, args.hostname,
                                       args.port, args.username,
                                       exception_handler=exit,
                                       exception_handler_args=(1,),
                                       exception_log=exception_log)
            else:
                download_specific_or_develop(ssh, plan,
                                             os.getenv(BAMBOO_BRANCH_NAME),
                                             args.hostname, args.port,
                                             args.username)
    ssh.close()


if __name__ == '__main__':
    main()
