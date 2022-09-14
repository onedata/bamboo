#!/usr/bin/env python

"""Author: Michal Stanisz
Copyright (C) 2020 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Runs integration tests, providing Erlang's ct_run with every
environmental argument it needs for successful run. The output is put into
'test_distributed/logs'. The (init|end)_per_suite "testcases" are removed from
the surefire.xml output.
This script is similar to ct_run.py but starts environment using onenv and k8s.

All paths used are relative to script's path, not to the running user's CWD.
Run the script with -h flag to learn about script's running options.
"""

from __future__ import print_function

from os.path import expanduser
import argparse
import os
import platform
import re
import sys
import glob
import xml.etree.ElementTree as ElementTree

from environment import docker, dockers_config
from environment.common import HOST_STORAGE_PATH, remove_dockers_and_volumes
import images_branch_config

script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(script_dir, 'bamboos/docker'))

CONFIG_DIRS = ['.docker', '.kube', '.minikube', '.one-env']
COVER_SPEC = 'cover.spec'
COVER_TMP_SPEC = 'cover_tmp.spec'


def main():
    parser = argparse.ArgumentParser(
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
        description='Run Common Tests.')

    parser.add_argument(
        '--image', '-i',
        action='store',
        default=None,
        help='override of docker image to use as test master.',
        dest='image')

    parser.add_argument(
        '--suite', '-s',
        action='append',
        help='name of the test suite',
        dest='suites')

    parser.add_argument(
        '--case', '-c',
        action='append',
        help='name of the test case',
        dest='cases')

    parser.add_argument(
        '--path-to-sources',
        default=os.path.normpath(os.path.join(os.getcwd(), '..')),
        help='path ot sources to be mounted in onenv container. '
             'Use when sources are outside HOME directory',
        dest='path_to_sources')

    parser.add_argument(
        '--no-clean',
        action='store_true',
        help='if set environment will not be cleaned up after tests',
        dest='no_clean')

    parser.add_argument(
        '--rsync',
        action='store_true',
        help='use rsync instead of local volume mount for deployments from sources',
        dest='rsync')

    parser.add_argument(
        '--performance', '-p',
        action='store_true',
        default=False,
        help='run performance tests',
        dest='performance')

    parser.add_argument(
        '--cover',
        action='store_true',
        default=False,
        help='run cover analysis',
        dest='cover')

    parser.add_argument(
        '-sf', '--sources-filter',
        action='append',
        help='Sources filter passed to onenv up script. Can be provided multiple times.'
    )

    parser.add_argument(
        '-zi', '--onezone-image',
        help='onezone image to use',
        dest='onezone_image'
    )

    parser.add_argument(
        '-pi', '--oneprovider-image',
        help='oneprovider image to use',
        dest='oneprovider_image'
    )

    parser.add_argument(
        '--no-pull',
        action='store_true',
        help='By default all tests scenarios force pulling docker images '
             'even if they are already present on host machine. When this '
             'option is passed no images will be downloaded.',
        dest='no_pull')

    args = parser.parse_args()
    dockers_config.ensure_image(args, 'image', 'worker')

    if 'bamboo_coverOptionOverride' in os.environ:
        print("----------------------------------------------------")
        if os.environ['bamboo_coverOptionOverride'] == "true":
            print("NOTE: overriding cover option to 'true' according to ${bamboo_coverOptionOverride} ENV variable")
            args.cover = True
        elif os.environ['bamboo_coverOptionOverride'] == "false":
            print("NOTE: overriding cover option to 'false' according to ${bamboo_coverOptionOverride} ENV variable")
            args.cover = False
        elif os.environ['bamboo_coverOptionOverride'] == "develop_only":
            if 'bamboo_planRepository_branchName' in os.environ and \
                os.environ['bamboo_planRepository_branchName'] == "develop":

                print(
                    "NOTE: overriding cover option to 'true' for branch 'develop' "
                    "according to ${bamboo_coverOptionOverride} ENV variable"
                )
                args.cover = True
            else:
                print(
                    "NOTE: overriding cover option to 'false' for branch other than 'develop' "
                    "according to ${bamboo_coverOptionOverride} ENV variable"
                )
                args.cover = False
        else:
            print("WARNING: ignoring bad value for ${{bamboo_coverOptionOverride}} ENV variable - '{}'".format(
                os.environ['bamboo_coverOptionOverride']
            ))
        print("----------------------------------------------------")
        sys.stdout.flush()

    if args.cover:
        print("----------------------------------------------------")
        print("Cover is enabled, the tests will take longer to run due to setup and later analysis.")
        print("----------------------------------------------------")
        sys.stdout.flush()
        prepare_cover()

    command = prepare_docker_command(args)
    remove_dockers_and_volumes()
    ret = start_test_docker(command, args)

    if ret != 0 and not skipped_test_exists(
            os.path.join(script_dir, "test_distributed/logs/*/surefire.xml")):
        ret = 0

    sys.exit(ret)


def prepare_ct_command(args):
    ct_command = ['ct_run',
                  '-abort_if_missing_suites',
                  '-dir', '.',
                  '-logdir', './logs/',
                  '-ct_hooks', 'cth_surefire', '[{path, "surefire.xml"}]',
                  'and', 'cth_logger',
                  'and', 'cth_onenv_up',
                  'and', 'cth_mock',
                  'and', 'cth_posthook',
                  '-noshell',
                  '-name', 'testmaster@testmaster',
                  '-hidden',
                  '-include', './include', '../include', '../_build/default/lib']

    code_paths = ['-pa']

    code_paths.extend(
        glob.glob(os.path.join(script_dir, '_build/default/lib', '*', 'ebin')))
    ct_command.extend(code_paths)

    ct_command.extend(['-env', 'path_to_sources',
                       os.path.normpath(os.path.join(os.getcwd(), args.path_to_sources))])

    ct_command.extend(['-env', 'clean_env', "false" if args.no_clean else "true"])
    ct_command.extend(['-env', 'rsync', "true" if args.rsync else "false"])
    ct_command.extend(['-env', 'cover', "true" if args.cover else "false"])
    if args.sources_filter:
        ct_command.extend(['-env', 'sources_filters', ';'.join(args.sources_filter)])
    ct_command.extend(['-env', 'onezone_image',
                       prepare_image(args.onezone_image, 'onezone', not args.no_pull)])
    ct_command.extend(['-env', 'oneprovider_image',
                       prepare_image(args.oneprovider_image, 'oneprovider', not args.no_pull)])

    if args.suites:
        ct_command.append('-suite')
        ct_command.extend([locate_suite(s) for s in args.suites])

    if args.cases:
        ct_command.append('-case')
        ct_command.extend(args.cases)

    if args.cover:
        ct_command.extend(['-cover', COVER_TMP_SPEC])

    if args.performance:
        ct_command.extend(['-env', 'performance', 'true'])

    return ct_command


def prepare_image(image, service_name, pull):
    if not image:
        image = images_branch_config.resolve_image(service_name)
    print('\n[INFO] Using image {} for service {}'.format(image, service_name))
    if pull:
        docker.pull_image_with_retries(image)
    return image


def prepare_docker_command(args):
    ct_command = prepare_ct_command(args)

    command = '''
import os, shutil, subprocess, sys, stat

home = '{user_home}'
os.environ['HOME'] = home
if not os.path.exists(home):
    os.makedirs(home)

if {shed_privileges}:
    os.environ['PATH'] = os.environ['PATH'].replace('sbin', 'bin')
    os.chown(home, {uid}, {gid})
    docker_gid = os.stat('/var/run/docker.sock').st_gid
    os.chmod('/etc/hosts', 0o666)
    os.chmod('/etc/resolv.conf', 0o666)
    os.setgroups([docker_gid])
    os.setregid({gid}, {gid})
    os.setreuid({uid}, {uid})

config_dirs={config_dirs}

# Try to copy config dirs, continue if it fails (might not exist on host).
for dirname in config_dirs:
    try:
        shutil.copytree(os.path.join('/tmp', dirname), os.path.join(home, dirname))
    except:
        pass

command = {cmd}
ret = subprocess.call(command)

import xml.etree.ElementTree as ElementTree, glob, re
for file in glob.glob('logs/*/surefire.xml'):
    tree = ElementTree.parse(file)
    for suite in tree.findall('.//testsuite'):
        for test in suite.findall('testcase'):
            match = re.match('(init|end)_per_suite', test.attrib['name'])
            if match is not None:
                suite.remove(test)
    tree.write(file)

sys.exit(ret)
    '''
    return command.format(
        uid=os.geteuid(),
        gid=os.getegid(),
        cmd=ct_command,
        user_home=expanduser('~'),
        config_dirs=CONFIG_DIRS,
        shed_privileges=(platform.system() == 'Linux'))


def start_test_docker(command, args):
    volumes = []
    for dirname in CONFIG_DIRS:
        path = expanduser(os.path.join('~', dirname))
        if os.path.isdir(path):
            volumes += [(path, os.path.join('/tmp', dirname), 'ro')]

    return docker.run(tty=True,
                      rm=True,
                      interactive=True,
                      workdir=os.path.join(script_dir, 'test_distributed'),
                      volumes=volumes,
                      reflect=[
                          (args.path_to_sources, 'rw'),
                          (script_dir, 'rw'),
                          ('/var/run/docker.sock', 'rw'),
                          (HOST_STORAGE_PATH, 'rw'),
                          ('/etc/passwd', 'ro')
                      ],
                      name='testmaster',
                      hostname='testmaster.test',
                      image=args.image,
                      command=['python', '-c', command])


def skipped_test_exists(junit_report_path):
    reports = glob.glob(junit_report_path)
    # if there are many reports, check only the last one
    reports.sort()
    tree = ElementTree.parse(reports[-1])
    testsuites = tree.getroot()
    for testsuite in testsuites:
        if testsuite.attrib['skipped'] != '0':
            return True
    return False


def prepare_cover():
    excl_mods = glob.glob(
        os.path.join(script_dir, 'test_distributed', '*.erl'))
    excl_mods = [os.path.basename(item)[:-4] for item in excl_mods]
    cover_template = os.path.join(script_dir, 'test_distributed', COVER_SPEC)
    new_cover = os.path.join(script_dir, 'test_distributed', COVER_TMP_SPEC)

    incl_dirs = []
    with open(cover_template, 'r') as template, open(new_cover, 'w') as cover:
        for line in template:
            if 'incl_dirs_r' in line:
                dirs_string = re.search(r'\[(.*?)\]', line).group(1)
                incl_dirs = [os.path.join(script_dir, d[1:]) for d in
                             dirs_string.split(', ')]
            elif 'excl_mods' in line:
                modules_string = re.search(r'\[(.*?)\]', line).group(1)
                excl_mods.extend([d.strip('"') for d in modules_string.split(', ')])
            else:
                print(line, file=cover)

        print('{{incl_dirs_r, ["{0}]}}.'.format(', "'.join(incl_dirs)), file=cover)
        print('{{excl_mods, [{0}]}}.'.format(', '.join(excl_mods)), file=cover)


def locate_suite(name):
    if '/' in name:
        print(
            'NOTE: it is no longer required to provide full path(s) to the suite(s) you wish to run. '
            'It is enough to provide the suite name (without the "_test_SUITE" suffix).'
        )
        name = os.path.basename(name)
    if '_test_SUITE' not in name:
        name += '_test_SUITE'
    if '.erl' not in name:
        name += '.erl'
    return find_suite_file(name)


def find_suite_file(name):
    for root, dirs, files in os.walk('test_distributed'):
        if name in files:
            return os.path.relpath(os.path.join(root, name), 'test_distributed')
    print('ERROR: Suite with name {} has not been found in the test_distributed directory'.format(name))
    sys.exit(1)


if __name__ == '__main__':
    main()
