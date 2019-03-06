#!/bin/bash

TIMEOUT=300
BAMBOO_USER="bamboo"
UBUNTU_USER="ubuntu"
BAMBOO_HOME="/home/bamboo"
UBUNTU_HOME="/home/ubuntu"

print_horizontal_line() {
    printf '%*s\n' "${COLUMNS:-$(tput cols || echo 100)}" '' | tr ' ' -
}

execute() {
  MSG=""
  CMD=$@

  echo ""
  echo "Executing: ${CMD}"

  ${CMD}
  CMD_CODE=$?
  if [ ${CMD_CODE} != 0 ]; then
    echo "ERROR: Command failed: ${CMD}"
    [ "$MSG" != "" ] && echo "Last message: ${MSG}"
    exit 1
  fi

  print_horizontal_line
}

GET_PODS_CMD="sudo -u ${BAMBOO_USER} kubectl get pods"
echo "Executing: ${GET_PODS_CMD}"
${GET_PODS_CMD}
GET_PODS_CMD_CODE=$?
print_horizontal_line

if [ ${GET_PODS_CMD_CODE} -eq 0 ]; then
    echo "K8s is running."
else
    echo "K8s is not running."
    echo "Restarting..."
	sudo minikube delete
	export MINIKUBE_HOME=${BAMBOO_HOME}
    export KUBECONFIG=${BAMBOO_HOME}/.kube/config
	execute sudo minikube start --vm-driver none --kubernetes-version v1.13.0 \
	    --extra-config=kubelet.eviction-hard="imagefs.available<5%,nodefs.available<5%,"
    execute sudo cp -r ${BAMBOO_HOME}/.kube ${UBUNTU_HOME}
    execute sudo cp -r ${BAMBOO_HOME}/.minikube ${UBUNTU_HOME}
    execute sed -i "s|${BAMBOO_HOME}|${UBUNTU_HOME}|g" ${UBUNTU_HOME}/.kube/config
    execute sudo chown -R ${UBUNTU_USER} ${UBUNTU_HOME}/.kube
    execute sudo chown -R   ${UBUNTU_USER} ${UBUNTU_HOME}/.minikube

	execute kubectl create clusterrolebinding serviceaccounts-cluster-admin \
	  --clusterrole=cluster-admin \
	  --group=system:serviceaccounts

	echo "K8s started successfully!"
fi


HELM_LS_CMD="sudo -u ${BAMBOO_USER} helm ls"
echo ""
echo "Executing: ${HELM_LS_CMD}"
${HELM_LS_CMD}
HELM_LS_CMD_CODE=$?
print_horizontal_line

TIME=0

if [ ${HELM_LS_CMD_CODE} -eq 0 ]; then
    echo "Helm is running."
else
	echo "Helm is not running."
	echo "Initializing helm..."

	execute helm init

	while [ ${TIME} -le ${TIMEOUT} ] && [ ${HELM_LS_CMD_CODE} -ne 0 ]; do
		sleep 1
		TIME=$((TIME+1))
		LOG=$(sudo -u ${BAMBOO_USER} helm ls 2>&1)
		HELM_LS_CMD_CODE=$?
	done

	if [ ${HELM_LS_CMD_CODE} -ne 0 ]; then
		echo "Helm couldn't start in ${TIMEOUT} seconds."
		echo "Log: ${LOG}"
		exit 1
	else
		echo "Helm started successfully!"
	fi
fi
