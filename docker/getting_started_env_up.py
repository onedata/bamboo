# onepanel_client is generated after running onedata's makefile with command make swaggers
import sys
sys.path.append('.')

try:
    from tests.gui.onepanel_client import ApiClient as ApiClient_OP
    from tests.gui.onepanel_client import OnepanelApi
    from tests.gui.onepanel_client.configuration import Configuration as Conf_Onepanel
    from tests.gui.onepanel_client import OneproviderApi
    from tests.gui.onepanel_client import ProviderModifyRequest
except ImportError:
    print 'You have to generate swagger clients using \'make swaggers\' command in onedata directory'
    exit(1)

from bamboos.docker.environment import docker
from subprocess import Popen, PIPE, STDOUT, call
import re
import time
import argparse
import os
import json


def print_logs(client_name, client_docker_logs):
    print client_name + ' docker logs '
    print client_docker_logs

    path = 'getting_started/scenarios/2_0_oneprovider_onezone/config_' + client_name + '/var/log/'
    try:
        directories = os.listdir(path)
    except IOError:
        print 'Couldn\'t find ' + path
        return

    for dir in directories:
        try:
            files = os.listdir(path + dir)
        except IOError:
            print 'Couldn\'t find ' + path + dir
            break
        for file in files:
            try:
                with open(path + dir + '/' + file, 'r') as logs:
                    print client_name + ' ' + dir + ' ' + file
                    print logs.readlines()
            except IOError:
                print 'Couldn\'t find ' + path + dir + '/' + file


def start_client(start_client_path, start_client_args, client_name, timeout):
    client_process = Popen(['./run_onedata.sh'] + start_client_args, stdout=PIPE, stderr=STDOUT,
                           cwd=start_client_path)
    client_process.wait()
    client_output = client_process.communicate()[0]
    print client_output
    splited_client_output = client_output.split('\n')
    docker_name = [item for item in splited_client_output if re.search('Creating ' + client_name, item)]
    docker_name = docker_name[0].split()
    docker_name = docker_name[len(docker_name) - 1]

    # Wait for client to start
    client_docker_logs = ''
    check_if_client_is_up = ['docker', 'logs', docker_name]
    timeout = time.time() + timeout
    while not (re.search('Congratulations', client_docker_logs)):
        client_process = Popen(check_if_client_is_up, stdout=PIPE, stderr=STDOUT)
        client_process.wait()
        client_docker_logs = client_process.communicate()[0]
        if re.search('Error', client_docker_logs):
            print 'Error while starting ' + client_name
            print_logs(client_name, client_docker_logs)
            exit(1)
        if time.time() > timeout:
            print 'Timeout while starting ' + client_name
            print_logs(client_name, client_docker_logs)
            exit(1)
        time.sleep(2)
    print client_name + ' has started'

    # Get ip of client
    client_docker_logs = client_docker_logs.split('\n')
    client_ip = [item for item in client_docker_logs if re.search('IP Address', item)]
    if len(client_ip) == 0:
        print 'Couldn\'t find ' + client_name + ' IP address'
        print_logs(client_name, client_docker_logs)
        exit(1)
    client_ip = client_ip[0].split()
    client_ip = client_ip[len(client_ip) - 1]
    print client_name + ' IP: ' + str(client_ip)
    return client_ip


parser = argparse.ArgumentParser()
parser.add_argument('--admin-credentials', action='store', default='admin1:Password1',
                    help='Username and password for admin user', required=False)
parser.add_argument('--docker-name', action='store', default='',
                    help='Name of docker that will be added to network', required=False)
args = parser.parse_args()


start_clients_path = 'getting_started/scenarios/2_0_oneprovider_onezone'
start_onezone_args = ['--zone', '--detach', '--with-clean']
start_oneprovider_args = ['--provider', '--detach', '--without-clean']
timeout = 60 * 5

print 'Starting onezone'
onezone_ip = start_client(start_clients_path, start_onezone_args, 'onezone', timeout)
print 'Starting oneprovider'
oneprovider_ip = start_client(start_clients_path, start_oneprovider_args, 'oneprovider', timeout)

if args.docker_name:
    docker.connect_docker_to_network('20oneprovideronezone_scenario2', args.docker_name)

# Configure environment
print 'Configuring environment'
onezone_address = 'https://' + onezone_ip
oneprovider_address = 'https://' + oneprovider_ip

# Onepanel operations
OZ_REST_PORT = 8443
PANEL_REST_PORT = 9443
PANEL_REST_PATH_PREFIX = "/api/v3/onepanel"
OZ_REST_PATH_PREFIX = "/api/v3/onezone"

# Create actual REST API endpoint for onepanel
Onepanel_REST_ENDPOINT = onezone_address + ':' + str(PANEL_REST_PORT) + PANEL_REST_PATH_PREFIX
Oneprovider_REST_ENDPOINT = oneprovider_address + ':' + str(PANEL_REST_PORT) + PANEL_REST_PATH_PREFIX

print Onepanel_REST_ENDPOINT, Oneprovider_REST_ENDPOINT
# Only necessary when connecting to a private Onezone instance
Conf_Onepanel().verify_ssl = False

# Set Configuration in Onepanel for admin 'admin'
username, password = args.admin_credentials.split(':')
USERNAME = username
PASSWORD = password
Conf_Onepanel().username = USERNAME
Conf_Onepanel().password = PASSWORD

# Login as admin 'admin' to oneprovider panel
oneprovider_client = ApiClient_OP(host=Oneprovider_REST_ENDPOINT,
                                  header_name='authorization',
                                  header_value=Conf_Onepanel().get_basic_auth_token())

# Create oneprovider api for admin 'admin'
oneprovider_api = OneproviderApi(oneprovider_client)

# Change provider redirection point and name
provider_modify_request = ProviderModifyRequest(redirection_point=oneprovider_address)
oneprovider_api.patch_provider(provider_modify_request)

output = {
    'oneprovider_host': oneprovider_ip,
    'onezone_host': onezone_ip,
    'op_panel_host': oneprovider_ip,
    'oz_panel_host': onezone_ip
}

print json.dumps(output)