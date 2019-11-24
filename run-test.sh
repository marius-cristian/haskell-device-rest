#!/bin/bash

export motoEnv="test"
sleep 4
stack test --test-arguments "$1"

# used only with inotify to reload on file change detection. the image used doesnt have it installed
# while true
# do
    # inotifywait -r -e "modify,delete" ./src ./test
    # stack test --test-arguments "$1"
# done
