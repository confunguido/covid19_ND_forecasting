#!/bin/bash
## The reason to have this file is to create a new file with a date if today is more recent than last time the code was run
## There might be a better way to do this, until then, this is it
if [ ! -f $1 ]; then
    echo $(date +"%Y-%m-%d") | cat > $1
else
    [[ $(date -d $(cat $1) +"%Y-%m-%d") < $(date +"%Y-%m-%d") ]] && echo $(date +"%Y-%m-%d") | cat > $1
fi
