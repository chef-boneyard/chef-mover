#!/usr/bin/env bash
#
# convert the filesystem from ext3 to ext4 in-place
#

set -x -e

COUCH_DATA=/srv/couch-data
COUCH_DEVICE=/dev/couchdb/couchdb

handle_errors()
{
    if [ $? -ne 1 ];
    then
        exit_code=$1
        echo "Failedq $exit_code"
        exit $exit_code
    fi
}

# stop couchdb
sudo /etc/init.d/couchdb stop

# convert the filesystem
umount $COUCH_DEVICE
tune2fs -O extents,dir_index,dir_nlink $COUCH_DEVICE
mount $COUCH_DEVICE $COUCH_DATA

# start couchdb
sudo /etc/init.d/couchdb start
