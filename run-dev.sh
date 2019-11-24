#!/bin/bash
#
# do not use this directly, it is called  from the docker-compose-dev file
#
#
export motoEnv="dev"
stack build && stack exec motorola-challenge-exe &

# used only with inotify to reload on file change detection. the image used doesnt have it installed
# while true
# do
    # inotifywait -r -e "modify,delete" ./src
    # ps aux | grep motorola-challenge-exe
    # killall motorola-challenge-exe || :
    # killall stack || :
    # echo "RESTARTING **************''"
    # stack build --allow-different-user --profile && stack exec -- motorola-challenge-exe &
# done
