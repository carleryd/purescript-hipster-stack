inotifywait -q -m -e close_write src/* |
    while read -r filename event; do
        ./build.sh
    done
