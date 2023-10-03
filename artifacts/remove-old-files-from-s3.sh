#!/bin/bash
#
# This script removes old files from an s3 bucket. It is intended for
# cleanups of bamboo artifacts repo. It will omit release branches artifacts.
#
# Forked from https://gist.github.com/JProffitt71/9044744
#
# Usage: ./remove-old-files-from-s3.sh "<bucket>" "<file age limit>"
# Example: ./remove-old-files-from-s3.sh "bamboo-artifacts-2" "1 year"
#
# Note: Your valid credentials should be placed in ~/.s3cfg
 
olderThan=$(date -d "$2 ago" "+%s")
now=$(date "+%s")
if [[ $(($now - $olderThan)) -lt 31536000 ]]; then     # 31536000 = 1 year
    echo "You specified less than one year for age of files to be removed"
    echo "Is this what you want? (yes/no)"
    read ans
    if [[ x$ans != "xyes" ]]; then
	echo "Exiting..."
	exit 1
    fi
fi

echo "Removing files older than $2 ($olderThan seconds since epoch)..."
s3cmd ls -r s3://$1 | grep " DIR " -v | while read -r line; do
    createDate=`echo $line|awk {'print $1" "$2'}`
    createDate=$(date -d "$createDate" "+%s")
    if [[ $createDate -lt $olderThan ]]; then
        fileName=`echo $line|awk {'print $4'}`
        if ! [[ $fileName =~ "/release/" ]]; then
            printf 'Deleting "%s"\n' $fileName
            s3cmd del "$fileName"
        fi
    fi
done;
