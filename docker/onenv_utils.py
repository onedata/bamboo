"""This file contains utility functions for onenv environments.
"""
__author__ = "Michal Stanisz"
__copyright__ = "Copyright (C) 2022 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in " \
              "LICENSE.txt"

import os
import yaml
import subprocess

from docker_build import cmd, get_branch_tag
from environment import docker

SERVICE_TO_IMAGE = {
    'onezone': 'docker.onedata.org/onezone-dev',
    'oneprovider': 'docker.onedata.org/oneprovider-dev',
    'oneclient': 'docker.onedata.org/oneclient-dev',
    'rest_cli': 'docker.onedata.org/rest-cli'
}

PULL_DOCKER_IMAGE_RETRIES = 5


def get_image_from_branch_config(service, pull=True, fail_on_error=False):
    """Returns service image based on branch from branchConfig.yaml file"""
    branch_config_path = os.path.join(os.getcwd(), 'branchConfig.yaml')
    try:
        with open(branch_config_path, 'r') as branch_config_file:
            branch_config = yaml.load(branch_config_file, yaml.Loader)
            fallback_branch = branch_config['default']
            fallback_tag = get_branch_tag(fallback_branch)
            service_branch = branch_config['images'][service]
            if service_branch == 'current_branch':
                branch = cmd(['git', 'rev-parse', '--abbrev-ref', 'HEAD'])
                branch_tag = get_branch_tag(branch)
            elif service_branch == 'default':
                branch_tag = fallback_tag
            else:
                branch_tag = service_branch

            image = '{}:{}'.format(SERVICE_TO_IMAGE[service], branch_tag)
            fallback_image = '{}:{}'.format(SERVICE_TO_IMAGE[service], fallback_tag)

            final_image = image if docker.image_exists(image) else fallback_image
            if pull:
                print('\n[INFO] Trying to download image {} for service {}'.format(
                    final_image, service))
                pull_docker_image_with_retries(final_image)
            else:
                print('\n[INFO] Using image {} for service {}'.format(final_image, service))
            return final_image
    except (IOError, KeyError) as e:
        if fail_on_error:
            print("[ERROR] Error when reading image for {} from branch config file {}: {}.".format(
                service, branch_config_path, e))
            raise e
        print("[WARNING] Could not read image for '{}' from branch config file '{}': {}. Image "
              "provided in scenario yaml will be used.".format(service, branch_config_path, e))
        return None


def pull_docker_image_with_retries(image, retries=PULL_DOCKER_IMAGE_RETRIES):
    attempts = 0

    while attempts < retries:
        try:
            docker.pull_image(image)
        except subprocess.CalledProcessError as e:
            attempts += 1
            if attempts >= retries:
                print('Could not download image {}. Tried {} times. \n'
                      'Captured output from last call: {} \n'
                      .format(image, retries, e.output))
                raise
        else:
            return
