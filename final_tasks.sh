#!/usr/bin/env bash

ONEDATA_STORAGE_PATH="/tmp/onedata"
ONE_ENV_DEPLOYMENT_DIR="/home/bamboo/.one-env"

DOCKER_CMD_TIMEOUT=10
DELETE_HELM_RELEASE_TIMEOUT=60
DELETE_K8S_NAMESPACE_TIMEOUT=60
DELETE_K8S_ELEM_TIMEOUT=20


execute_with_timeout() {
    TIMEOUT=$1
    shift 1
    CMD=$@
    timeout --kill-after ${TIMEOUT} ${TIMEOUT} bash -c "${CMD}"
}


# clear spaces data and one-env deployment dir
echo "Clearing ${ONEDATA_STORAGE_PATH} and ${ONE_ENV_DEPLOYMENT_DIR}"
docker run -v ${ONEDATA_STORAGE_PATH}:${ONEDATA_STORAGE_PATH} \
           -v ${ONE_ENV_DEPLOYMENT_DIR}:${ONE_ENV_DEPLOYMENT_DIR} \
           alpine sh -c "rm -rf ${ONEDATA_STORAGE_PATH}/* ${ONE_ENV_DEPLOYMENT_DIR}/*"


# clear k8s
HELM_RELEASES=$(helm ls --all --short)
for release in ${HELM_RELEASES}
do
    execute_with_timeout ${DELETE_HELM_RELEASE_TIMEOUT} helm delete --purge ${release}
done

NAMESPACES=$(kubectl get ns -o jsonpath="{.items[*].metadata.name}" | grep -v kube-system)
for namespace in ${NAMESPACES};
do
    execute_with_timeout ${DELETE_K8S_NAMESPACE_TIMEOUT} kubectl delete ns ${namespace}
done

# pv are not in any namespace so we have to delete them separately
PVS=$(kubectl get pv --no-headers -o custom-columns=":metadata.name")
for pv in ${PVS}
do
    execute_with_timeout ${DELETE_K8S_ELEM_TIMEOUT} kubectl delete pv ${pv}
done

# sometimes deleting helm release / k8s namespace leaves some deployments
DEPLOYMENTS=$(kubectl get deployments --no-headers -o custom-columns=":metadata.name")
for deployment in ${DEPLOYMENTS}
do
    execute_with_timeout ${DELETE_K8S_ELEM_TIMEOUT} kubectl delete deployment ${deployment}
done

PODS=$(kubectl get pods --no-headers -o custom-columns=":metadata.name")
for pod in ${PODS}
do
    execute_with_timeout ${DELETE_K8S_ELEM_TIMEOUT} kubectl delete pod ${pod}
done

SERVICES=$(kubectl get services --no-headers -o custom-columns=":metadata.name" | grep -v kubernetes)
for service in ${SERVICES}
do
    execute_with_timeout ${DELETE_K8S_ELEM_TIMEOUT} kubectl delete service ${service}
done

# clear docker
CONTAINERS=$(docker ps -qa)
CONTAINERS_TO_REMOVE=${CONTAINERS}

for container in ${CONTAINERS}
do
    NAMESPACE=$(execute_with_timeout ${DOCKER_CMD_TIMEOUT} docker inspect --format '"{{ index .Config.Labels \"io.kubernetes.pod.namespace\"}}"' ${container})
    if [[ ${NAMESPACE} ]]
    then
        if [[ "${NAMESPACE}" ==  "kube-system" ]]
        then
            CONTAINERS_TO_REMOVE=( "${CONTAINERS_TO_REMOVE[@]/$container}" )
        fi
    fi
done

echo "Stalled docker containers to remove: "
echo ${CONTAINERS_TO_REMOVE}
echo "Removing stalled docker containers"

for container in ${CONTAINERS_TO_REMOVE}
do
    execute_with_timeout ${DOCKER_CMD_TIMEOUT} docker kill ${container}
    execute_with_timeout ${DOCKER_CMD_TIMEOUT} docker rm -fv ${container}
done

STALLED_DOCKER_VOLUMES=$(docker volume ls -q)

echo "Stalled docker volumes to remove: "
echo ${STALLED_DOCKER_VOLUMES}
echo "Removing stalled docker volumes"

for volume in ${STALLED_DOCKER_VOLUMES}
do
    execute_with_timeout ${DOCKER_CMD_TIMEOUT} docker volume rm ${volume}
done

echo "Done"
