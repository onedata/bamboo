#!/usr/bin/env python

# coding=utf-8
"""Author: Bartek Kryza
Copyright (C) 2018 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

This script can be used to clean package repositories (aptly, rpm) by removing
old packages matching specific patterns.

Example use:
    # Clean xenial onezone packages older than 7 days matching version 18.02.0.rc*
    ./pkg_clean.py xenial oz-worker,oz-panel,onezone 18.02.0.rc 7

    # Clean CentOS onezone packages older than 7 days matching version 18.02.0.rc*
    ./pkg_clean.py centos oz-worker,oz-panel,onezone 18.02.0.rc 7
"""

import argparse
import json
import os
import re
import shutil
import sys
import tempfile
import time
import logging as LOG
from subprocess import check_output


def apt_clean_package(repo, name, version, older_than_days):
    """
    Remove all packages (binary and source) of a specific package
    identified by name and a version (can be wildcarded) in a repo.

    Example usage:
      apt_clean_package('xenial', 'op-panel', '18.02.0.rc', 7)
    """

    package_dir = os.path.join('/aptly/public', repo, 'pool/main', name[0], name)
    package_files = [f for f in os.listdir(package_dir)
                if re.match("{}_{}.*.dsc".format(name, version), f)]

    package_versions = set()
    for f in package_files:
        creation_time = os.path.getctime(os.path.join(package_dir, f))
        file_age_in_days = (time.time() - creation_time)/(24 * 3600)
        if file_age_in_days >= older_than_days:
            package_version = str(f).replace('.dsc', '').replace(name+'_', '')
            package_versions.add(package_version)

    for package_version in package_versions:
        LOG.info("=== Removing apt package {}_{} ===".format(name, package_version))
        result = check_output(
                ['aptly', 'repo', 'remove', '-dry-run=false', repo,
                    "{} (={})".format(name, package_version)])
        LOG.info(result)


def apt_clean_packages(repo, packages, version, days):
    """
    Clean specified apt packages
    """

    for package in packages:
        apt_clean_package(repo, package, version, days)


def apt_db_update(repo):
    """
    Update aptly database and republish repository
    """

    LOG.info("=== Updating aptly database ===")
    result_cleanup = check_output(['aptly', 'db', 'cleanup'])
    LOG.info(result_cleanup)
    result_update = check_output(
            ['aptly', 'publish', 'update', repo, repo])
    LOG.info(result_update)


def yum_clean_package(repo, name, version, older_than_days):
    """
    Remove all packages (binary and source) of a specific package
    identified by name and a version (can be wildcarded) in a repo.

    Example usage:
      yum_clean_package('centos/7x', 'op-panel', '18.02.0.rc', 7)
    """

    bin_package_dir = os.path.join('/var/www/onedata/yum', repo, 'x86_64')
    package_files = [f for f in os.listdir(bin_package_dir)
                if re.match("{}-{}.*".format(name, version), f)]

    for p in package_files:
        creation_time = os.path.getctime(os.path.join(bin_package_dir, p))
        file_age_in_days = (time.time() - creation_time)/(24 * 3600)
        if file_age_in_days >= older_than_days:
            LOG.info("=== Removing binary yum package {} which is {} days old"
                     .format(str(p), str(int(file_age_in_days))))
            os.remove(os.path.join(bin_package_dir, p))

    src_package_dir = os.path.join('/var/www/onedata/yum', repo, 'SRPMS')
    package_files = [f for f in os.listdir(src_package_dir)
                if re.match("{}-{}.*".format(name, version), f)]
    for p in package_files:
        creation_time = os.path.getctime(os.path.join(src_package_dir, p))
        file_age_in_days = (time.time() - creation_time)/(24 * 3600)
        if file_age_in_days >= older_than_days:
            LOG.info("=== Removing source yum package {} which is {} days old"
                     .format(str(p), str(int(file_age_in_days))))
            os.remove(os.path.join(src_package_dir, p))


def yum_clean_packages(repo, packages, version, days):
    """
    Clean specified yum packages
    """

    for package in packages:
        yum_clean_package(repo, package, version, days)


def yum_db_update(repo):
    """
    Update yum database and republish repository
    """

    LOG.info("=== Updating yum database ===")
    result_update = check_output(
        ['createrepo', '--update', os.path.join('/var/www/onedata/yum', repo)])
    LOG.info(result_update)


if __name__ == "__main__":

    if(len(sys.argv) != 5):
        print """
Script for cleaning the packages in aptly and rpm repositories, matching specific
versions and older than specified number of days.

Actions are logged in /var/log/pkg_clean.log

Example use:

    # Clean xenial onezone packages older than 7 days matching version 18.02.0.rc*
    ./pkg_clean.py xenial oz-worker,oz-panel,onezone 18.02.0.rc 7

    # Clean CentOS onezone packages older than 7 days matching version 18.02.0.rc*
    ./pkg_clean.py centos/7x oz-worker,oz-panel,onezone 18.02.0.rc 7
"""
        sys.exit(1)

    LOG.basicConfig(filename='/var/log/pkg_clean.log',
            format='%(asctime)s %(levelname)s %(message)s', level=LOG.INFO)
    LOG.getLogger().addHandler(LOG.StreamHandler())

    repo = sys.argv[1]
    packages = sys.argv[2].split(',')
    version = sys.argv[3]
    days = int(sys.argv[4])

    LOG.info("Package cleanup called for {}:{}:{}:{}".format(repo,str(packages),version,str(days)))

    if(repo == 'xenial'):
        apt_clean_packages(repo, packages, version, days)
        apt_db_update(repo)
    elif(repo == 'centos/7x'):
        yum_clean_packages(repo, packages, version, days)
        yum_db_update(repo)
    else:
        print "Invalid repo "+repo+". Supported repositories are: xenial, centos/7x"
        LOG.error("Invalid repo "+repo+". Supported repositories are: xenial, centos/7x")
        sys.exit(1)
