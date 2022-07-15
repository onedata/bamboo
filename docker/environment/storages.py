# coding=utf-8
"""Authors: Michal Wrona
Copyright (C) 2016 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Contains methods used to bring up storages.
"""
import os
import sys
import tempfile

from . import common, s3, ceph, cephrados, glusterfs, webdav, xrootd, nfs, http, amazon_iam, swift


def start_storages(config, config_path, ceph_image, cephrados_image, s3_image,
                    swift_image, glusterfs_image, webdav_image, xrootd_image,
                    nfs_image, http_image,image, uid):
    storages_dockers = {'ceph': {}, 'cephrados': {}, 's3': {}, 'posix': {},
            'swift': {}, 'glusterfs': {}, 'webdav': {}, 'xrootd': {}, 'nfs': {}, 'http': {}}
    docker_ids = []
    if 'os_configs' in config:
        start_iam_mock = False
        for key, cfg in config['os_configs'].items():
            for storage in cfg['storages']:
                if isinstance(storage, str):
                    sys.stderr.write('''WARNING:
        Detected deprecated syntax at os_configs.{0}.storages
        Change entry "{1}" to: {{ "type": "posix", "name": "{1}" }}
        In file {2}'''.format(key, storage, config_path))
                    break

                if storage['type'] == 'ceph' and storage['name'] not in \
                        storages_dockers['ceph']:
                    _ceph_up(storage, storages_dockers, ceph_image, docker_ids,
                             uid)

                elif storage['type'] == 'cephrados' and storage['name'] not in \
                        storages_dockers['cephrados']:
                    _cephrados_up(storage, storages_dockers, cephrados_image, docker_ids,
                             uid)

                elif storage['type'] == 's3' and storage['name'] not in \
                        storages_dockers['s3']:
                    start_iam_mock = _want_start_iam_mock(storage)
                    _s3_up(storage, storages_dockers, s3_image, docker_ids, uid)

                elif storage['type'] == 'swift' and storage['name'] not in \
                        storages_dockers['swift']:
                    _swift_up(storage, storages_dockers, swift_image,
                              docker_ids, uid)

                elif storage['type'] == 'nfs' and storage['name'] not in \
                        storages_dockers['nfs']:
                    _nfs_up(storage, storages_dockers, nfs_image, docker_ids,
                            uid, cfg)

                elif storage['type'] == 'glusterfs' and storage['name'] not in \
                        storages_dockers['glusterfs']:
                    _glusterfs_up(storage, storages_dockers, glusterfs_image,
                                  docker_ids, uid)

                elif storage['type'] == 'webdav' and storage['name'] not in \
                        storages_dockers['webdav']:
                    _webdav_up(storage, storages_dockers, webdav_image,
                               docker_ids, uid)

                elif storage['type'] == 'xrootd' and storage['name'] not in \
                        storages_dockers['xrootd']:
                    _xrootd_up(storage, storages_dockers, xrootd_image,
                               docker_ids, uid)

                elif storage['type'] == 'http' and storage['name'] not in \
                        storages_dockers['http']:
                    _http_up(storage, storages_dockers, http_image,
                               docker_ids, uid)

        if start_iam_mock:
            docker_ids.extend(_start_iam_mock(image, uid, storages_dockers))

    return storages_dockers, docker_ids


def _want_start_iam_mock(storage):
    return 'iam_host' not in storage and 'request_scheme' not in storage and \
           not storage.get('disable_iam_mock', False)


def _start_iam_mock(image, uid, storages_dockers):
    iam_mock_config = amazon_iam.up(image, uid)

    iam_request_scheme = 'http'
    iam_host = iam_mock_config['host_name']
    for key in list(storages_dockers['s3'].keys()):
        if 'iam_host' not in storages_dockers['s3'][key] and \
                        'request_scheme' not in storages_dockers['s3'][key]:
            storages_dockers['s3'][key]['iam_host'] = iam_host
            storages_dockers['s3'][key][
                'iam_request_scheme'] = iam_request_scheme

    return iam_mock_config['docker_ids']


def _ceph_up(storage, storages_dockers, ceph_image, docker_ids, uid):
    pool = tuple(storage['pool'].split(':'))
    result = ceph.up(ceph_image, [pool], storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']
    storages_dockers['ceph'][storage['name']] = result


def _cephrados_up(storage, storages_dockers, cephrados_image, docker_ids, uid):
    pool = tuple(storage['pool'].split(':'))
    result = cephrados.up(cephrados_image, [pool], storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']
    storages_dockers['cephrados'][storage['name']] = result


def _s3_up(storage, storages_dockers, s3_image, docker_ids, uid):
    result = s3.up(s3_image, [storage['bucket']],
                                   storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']

    if 'iam_host' in storage and 'iam_request_scheme' in storage:
        result['iam_host'] = storage['iam_host']
        result['iam_request_scheme'] = storage[
            'iam_request_scheme']

    storages_dockers['s3'][storage['name']] = result


def _swift_up(storage, storages_dockers, swift_image, docker_ids, uid):
    result = swift.up(swift_image, [storage['container']],
                                   storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']

    storages_dockers['swift'][storage['name']] = result


def _nfs_up(storage, storages_dockers, nfs_image, docker_ids, uid, cfg):

    tmp_dir = tempfile.mkdtemp(dir=common.HOST_STORAGE_PATH,
            prefix="nfs_helper_test_")
    os.chmod(tmp_dir, 0o777)

    result = nfs.up(nfs_image, uid, storage['name'], tmp_dir)
    docker_ids.extend(result['docker_ids'])

    del result['docker_ids']
    result['path'] = tmp_dir
    storages_dockers['nfs'][storage['name']] = result


def _glusterfs_up(storage, storages_dockers, glusterfs_image, docker_ids, uid):
    result = glusterfs.up(glusterfs_image, [storage['volume']], storage['name'],
                          uid, storage['transport'], storage['mountpoint'])
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']
    storages_dockers['glusterfs'][storage['name']] = result


def _webdav_up(storage, storages_dockers, webdav_image, docker_ids, uid):
    result = webdav.up(webdav_image, storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']
    storages_dockers['webdav'][storage['name']] = result


def _xrootd_up(storage, storages_dockers, xrootd_image, docker_ids, uid):
    result = xrootd.up(xrootd_image, storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']
    storages_dockers['xrootd'][storage['name']] = result


def _http_up(storage, storages_dockers, http_image, docker_ids, uid):
    result = http.up(http_image, storage['name'], uid)
    docker_ids.extend(result['docker_ids'])
    del result['docker_ids']
    storages_dockers['http'][storage['name']] = result


