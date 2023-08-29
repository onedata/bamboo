#!/usr/bin/env python3

"""Runs common tests in elixir using one-env.

The output is put into 'test_distributed/logs'. The (init|end)_per_suite
"testcases" are removed from the surefire.xml output.

All paths used are relative to script's path, not to the running user's CWD.
Run the script with -h flag to learn about script's running options.
"""

__author__ = "Bartosz Walkowicz"
__copyright__ = "Copyright (C) 2023 ACK CYFRONET AGH"
__license__ = "This software is released under the MIT license cited in LICENSE.txt"


import os
import sys

import ct_utils
from environment import docker, dockers_config
from environment.common import HOST_STORAGE_PATH, remove_dockers_and_volumes


SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(SCRIPT_DIR, "bamboos/docker"))


def main():
    args = ct_utils.parse_args()

    dockers_config.ensure_image(args, "image", "worker")
    remove_dockers_and_volumes()

    return_code = run_tests(args)

    if return_code != 0 and not ct_utils.any_test_skipped(
        os.path.join(SCRIPT_DIR, "test_distributed/logs/*/surefire.xml")
    ):
        return_code = 0

    sys.exit(return_code)


def run_tests(args):
    ct_cmd = prepare_ct_command(args)
    ct_env = ct_utils.prepare_ct_environment(args)

    return docker.run(
        tty=True,
        rm=True,
        interactive=True,
        workdir=SCRIPT_DIR,
        volumes=ct_utils.get_docker_volumes(),
        reflect=[
            (args.path_to_sources, "rw"),
            (SCRIPT_DIR, "rw"),
            ("/var/run/docker.sock", "rw"),
            (HOST_STORAGE_PATH, "rw"),
            ("/etc/passwd", "ro"),
        ],
        name="testmaster",
        hostname="testmaster.test",
        image=args.image,
        command=ct_utils.get_docker_command(ct_cmd, ct_env),
    )


def prepare_ct_command(args):
    cmd = [
        "elixir",
        "--hidden",
        "--name",
        "testmaster@testmaster",
        "-S",
        "mix",
        "ct",
    ]

    for suite in args.suites:
        cmd.extend(["--suite", ensure_full_suite_name(suite)])

    for group in args.groups:
        cmd.extend(["--group", group])

    for case in args.cases:
        cmd.extend(["--case", case])

    return cmd


def ensure_full_suite_name(suite):
    if not suite.endswith("TestSuite"):
        suite += "TestSuite"
    if not suite.startswith("Elixir."):
        suite = f"Elixir.{suite}"

    return suite


if __name__ == "__main__":
    main()
