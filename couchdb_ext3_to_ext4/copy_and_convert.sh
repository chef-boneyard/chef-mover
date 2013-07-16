#!/usr/bin/env bash
#
# copy all couchdb data from /srv/couch-data to root partition
# before converting to ext4 and copying back
#

set -x -e

TEMP_LOCATION=/tmp/couch-backup
COUCH_DATA=/srv/couch-data
COUCH_DEVICE=/dev/couchdb/couchdb

# remove old bacup data
#
# NOTE: by not deleting anything stored in the temp location
#       we can make use of rsync's incremental update to
#       speed up the conversion process
#
# rm -rf $TEMP_LOCATION

# create the temp directory
mkdir -p $TEMP_LOCATION

# stop couchdb
sudo /etc/init.d/couchdb stop

# copy to temp
rsync -a $COUCH_DATA/ $TEMP_LOCATION

# make the filesystem
umount $COUCH_DEVICE
mkfs.ext4 $COUCH_DEVICE
mount $COUCH_DEVICE $COUCH_DATA

# copy back
rsync -a $TEMP_LOCATION/ $COUCH_DATAq

# start couchdb
sudo /etc/init.d/couchdb start
