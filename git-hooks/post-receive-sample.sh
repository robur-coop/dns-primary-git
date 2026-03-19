#!/bin/sh

# A sample script, server-side, for post-receive on the git repository
# Put it as <REPO>/hooks/post-receive

# We sent a DNS notify to the primary server

PRIMARY_IP="127.0.0.1"

while read -r oldrev newrev _refname; do
    ZONE=
    for file in $(git diff --name-only "$oldrev" "$newrev"); do
        if [ "$file" = "_keys" ]; then
            # exclude the _keys zone
            continue
        else
            ZONE=$file
        fi
    done
    # filter out transfer keys
    KEYS=$(git show "$newrev":_keys | grep "$ZONE" | grep -v -E -e "([0-9]{1,3}\.){3}[0-9]{1,3}")
    # find zone key or root key
    KEY=$(echo "$KEYS" | grep "$ZONE" | head -1)
    if [ $? -eq 1 ]; then
        KEY=$(echo "$KEYS" | grep -e -E '\._update\. |\._transfer\. |\._notify\. ' | head -1)
    fi
    if [ $? -eq 1 ]; then
        echo "no key found for \"$ZONE\" (or root)";
        exit 1
    fi;
    KEY_NAME=$(echo "$KEY" | cut -d ' ' -f 1)
    KEY_VAL=$(echo "$KEY" | rev | cut -d ' ' -f 1 | rev)
    /usr/local/bin/onotify --key="$KEY_NAME":SHA256:"$KEY_VAL" "$PRIMARY_IP" "$ZONE"
    echo "notified \"$PRIMARY_IP\" for update of \"$ZONE\""
done
