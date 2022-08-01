# coding=utf-8
"""Authors: Łukasz Opioła
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a DNS server based on dnsmasq that allow sdifferent dockers
to see each other by hostnames. In addition, it allows for specifying
root servers for given domains, which is indispensable in distributed
providers <-> oz environment.
"""

import subprocess
from .timeouts import *
from . import common, docker, dockers_config


def _dns_ready(dns):
    ip = common.get_docker_ip(dns)
    hostname = common.get_docker_name(dns)
    # If the dns is working, it should return
    # its own ip when asked about its hostname
    try:
        result = subprocess.check_output(
            ['timeout', '10', 'dig', '+short', '@{0}'.format(ip), hostname],
            stdin=None, stderr=None)
        return result.strip('\n') == ip
    except:
        return False


def dns_hostname(uid):
    """Formats hostname for a docker hosting DNS.
    NOTE: Hostnames are also used as docker names!
    """
    return common.format_hostname('dns', uid)


def maybe_start(config, uid):
    """Sets up DNS configuration values, starting the server if needed."""
    if config == 'auto':
        dns_config = up(uid, [], [], 'none')
        return [dns_config['dns']], dns_config

    if config == 'none':
        return [], {}

    return [config], {}


def maybe_restart_with_configuration(config, uid, output):
    """If dns is set to 'auto' and there is at least one domain in the output,
    this will restart this server with root_servers specified in domains.
    output - the json generated by starting scripts."""
    if config == 'auto' and 'domains' in output and output['domains']:
        hosts = []
        dnses = []
        for domain in output['domains']:
            if 'a' in output['domains'][domain]:
                for ip in output['domains'][domain]['a']:
                    hosts.append('host/{0}/{1}'.format(domain, ip))
            if 'ns' in output['domains'][domain]:
                for ip in output['domains'][domain]['ns']:
                    dnses.append('dns/{0}/{1}'.format(domain, ip))
        up(uid, hosts, dnses, dns_hostname(uid))


def up(uid, hosts, dnses, dns_to_restart):
    dns = dns_to_restart
    if dns == 'none':
        # Start new DNS docker (just the docker, don't start the DNS server yet)
        dns = docker.run(
            image=dockers_config.get_image('dns'),
            name=dns_hostname(uid),
            detach=True,
            interactive=True,
            tty=True,
            reflect=[('/var/run/docker.sock', 'rw')],
            command=['bash'])

    # And start the DNS server. If the restart flag was set,
    # this will only restart the current server (on the same docker).
    _restart_with_configuration(dns, hosts, dnses)

    ip = common.get_docker_ip(dns)

    common.wait_until(_dns_ready, [dns], DNS_WAIT_SECONDS)

    return {'dns': ip, 'docker_ids': [dns]}


def _restart_with_configuration(dns, hosts, dnses):
    """Starts or restarts the DNS with given hosts (static A records)
    and dnses (static NS records). They are in format:
    ['host/domain.com/127.0.0.1', ...], ['dns/hostname.com/127.0.0.1', ...]
    Domains and servers can be repeated among different entries if needed.
    """
    command = ['/root/run.sh']
    command.extend(hosts)
    command.extend(dnses)
    docker.exec_(
        container=dns,
        detach=True,
        interactive=True,
        tty=True,
        command=' '.join(command))
