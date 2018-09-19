#!/usr/bin/env bash

ONEDATA_STORAGE_PATH="/tmp/onedata"

# clear spaces data
echo "Clearing ${ONEDATA_STORAGE_PATH}"
docker run -v ${ONEDATA_STORAGE_PATH}:${ONEDATA_STORAGE_PATH} alpine sh -c "rm -rf ${ONEDATA_STORAGE_PATH}/*"


K8S_CONTAINER_NAME_LABEL_KEY="io.kubernetes.container.name"
CONTAINERS_NAMES=$(docker ps --all --format "{{.Names}}")
CONTAINERS_TO_REMOVE=${CONTAINERS_NAMES}

for container_name in ${CONTAINERS_NAMES}
do
    if [[ ${container_name} == k8s* ]]
    then
         CONTAINERS_TO_REMOVE=( "${CONTAINERS_TO_REMOVE[@]/$container_name}" )
    fi
done

echo "Stalled docker containers to remove: "
echo ${CONTAINERS_TO_REMOVE}
echo "Removing stalled docker containers"

for container in ${CONTAINERS_TO_REMOVE}
do
    docker kill ${container}
    docker rm -fv ${container}
done


STALLED_DOCKER_VOLUMES=$(docker volume ls -q)

echo "Stalled docker volumes to remove: "
echo ${STALLED_DOCKER_VOLUMES}
echo "Removing stalled docker volumes"

for volume in ${STALLED_DOCKER_VOLUMES}
do
    docker volume rm ${volume}
done

echo "Done"
