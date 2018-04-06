#!/usr/bin/env bash

K8S_CONTAINER_NAME_LABEL_KEY="io.kubernetes.container.name"
CONTAINERS=$(docker ps -qa)
CONTAINERS_TO_REMOVE=${CONTAINERS}
ONEDATA_STORAGE_PATH="/tmp/onedata"


for container in ${CONTAINERS}
do
    if [ $(docker inspect --format "{{ index .Config.Labels \"io.kubernetes.container.name\"}}" ${container}) ]
    then
        CONTAINERS_TO_REMOVE=( "${CONTAINERS_TO_REMOVE[@]/$container}" )
    fi
done

echo ${CONTAINERS_TO_REMOVE}

for container in ${CONTAINERS_TO_REMOVE}
do
    docker kill ${container}
    docker rm -fv ${container}
done

#docker volume rm $(docker volume ls -q)


# clear spaces data
rm -rf ${ONEDATA_STORAGE_PATH}/*