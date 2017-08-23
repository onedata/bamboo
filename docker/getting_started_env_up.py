#!/usr/bin/env python
# coding=utf-8

"""Author: Michał Ćwiertnia
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

This file is mainly used in onedata tests.

Starts scenario 2.1 from onedata's getting started.
Runs isolated Onedata deployment consisting of:
- a single node preconfigured Onezone instance
- a single node preconfigured Oneprovider instance

To run this script manually:
- run script from onedata repo root dir
- make sure there is tests/gui directory in onedata repo root dir
- make sure you have python libraries: urllib3, certifi
- make sure you have getting_started, onezone_swagger and onepanel_swagger submodules
- build swagger clients running command: "make build_swaggers" from onedata repo root dir

Run the script with -h flag to learn about script's running options.
"""

import sys
sys.path.append('.')

from environment import docker
from subprocess import Popen, PIPE, STDOUT
import os
import re
import time
import json
import argparse


SCENARIO_PATH = os.path.join('getting_started', 'scenarios', '2_1_oneprovider_onezone_onepanel')
SCENARIO_NETWORK = '21oneprovideronezoneonepanel_scenario2'
TIMEOUT = 60 * 10

start_onezone_args = ['--zone', '--detach', '--with-clean']
start_oneprovider_args = ['--provider', '--detach', '--without-clean']


def print_logs(service_name, service_docker_logs):
    print '{} docker logs '.format(service_name)
    print service_docker_logs

    path = os.path.join(SCENARIO_PATH, 'config_' + service_name, 'var', 'log')
    try:
        directories = os.listdir(path)
    except IOError:
        print 'Couldn\'t find {}'.format(path)
    else:
        for directory in directories:
            try:
                files = os.listdir(os.path.join(path, directory))
            except IOError:
                print 'Couldn\'t find {}'.format(os.path.join(path, directory))
            else:
                for file in files:
                    try:
                        with open(os.path.join(path, directory, file), 'r') as logs:
                            print '{service_name} {dir} {file}'.format(service_name=service_name,
                                                                       dir=directory,
                                                                       file=file)
                            print logs.readlines()
                    except IOError:
                        print 'Couldn\'t find {}'.format(os.path.join(path, directory, file))


PERSISTENCE = ('# configuration persistance',
               '# data persistance',
               '# configuration persistence',
               '# data persistence')


def rm_persistence(path, service_name):
    """Remove persistence of configuration/data.
    """
    service_path = os.path.join(path, 'docker-compose-{}.yml'
                                      ''.format(service_name))
    with open(service_path, 'r+') as f:
        lines = f.readlines()

        comment = False
        for i, line in enumerate(lines):
            if comment:
                lines[i] = '#{}'.format(line) if line[0] != '#' else line
                comment = False
            if any(per in line for per in PERSISTENCE):
                comment = True

        f.seek(0)
        f.writelines(lines)


def start_service(start_service_path, start_service_args, service_name, timeout):
    """
    service_name argument is one of: onezone, oneprovider
    Runs ./run_onedata.sh script from onedata's getting started scenario 2.0
    Returns ip of started service
    """
    service_process = Popen(['./run_onedata.sh'] + start_service_args, stdout=PIPE, stderr=STDOUT,
                            cwd=start_service_path)
    service_output = service_process.communicate()[0]
    print service_output
    service = 'onezone' if service_name == 'oz_panel' else 'oneprovider'
    docker_name = re.search(r'Creating\s*(?P<name>{}[\w-]+)\b'.format(service),
                            service_output).group('name')

    timeout = time.time() + timeout

    # Get ip of service
    service_ip = None
    while not service_ip:
        service_process = Popen(['docker', 'logs', docker_name], stdout=PIPE,
                                stderr=STDOUT)
        docker_logs = service_process.communicate()[0]
        service_ip = re.search(r'IP Address:\s*(?P<ip>(\d{1,3}\.?){4})',
                               docker_logs)
        if re.search('Error', docker_logs):
            print 'Error while starting {}'.format(service_name)
            print_logs(service_name, docker_logs)
            exit(1)
        if time.time() > timeout:
            print 'Couldn\'t find {} IP address'.format(service_name)
            print_logs(service_name, docker_logs)
            exit(1)
        time.sleep(2)

    service_ip = service_ip.group('ip')
    print '{service_name} IP: {service_ip}'.format(service_name=service_name,
                                                   service_ip=service_ip)
    return service_ip


parser = argparse.ArgumentParser()
parser.add_argument('--docker-name', action='store', default='',
                    help='Name of docker that will be added to network', required=False)
args = parser.parse_args()

print 'Starting onezone panel'
rm_persistence(SCENARIO_PATH, 'onezone')
oz_panel_ip = start_service(SCENARIO_PATH, start_onezone_args, 'oz_panel', TIMEOUT)
print 'Starting oneprovider'
rm_persistence(SCENARIO_PATH, 'oneprovider')
op_panel_ip = start_service(SCENARIO_PATH, start_oneprovider_args, 'op_panel', TIMEOUT)

if args.docker_name:
    docker.connect_docker_to_network(SCENARIO_NETWORK, args.docker_name)

output = {
    'oneprovider_host': op_panel_ip,
    'onezone_host': oz_panel_ip,
    'op_panel_host': op_panel_ip,
    'oz_panel_host': oz_panel_ip
}

print json.dumps(output)
