#!/bin/sh

set -e

# A sample script, server-side, for pre-receive on the git repository
# Put it as <REPO>/hooks/pre-receive

# Input is any number of lines of the form "<oldref> <newref> <refname>"

temp_file=$(mktemp)
old_temp_file=$(mktemp)

cleanup () {
  rm -rf "${temp_file}" "${old_temp_file}"
}

trap cleanup EXIT

while read -r oldrev newrev _refname; do
    # added files
    git diff --diff-filter=A --name-only "$oldrev" "$newrev" | while read -r file; do
        if [ "$file" = "_keys" ]; then
            # exclude the _keys zone
            continue
        else
            echo "checking zone $file"
            git show "$newrev":"$file" > "$temp_file"
            ozone --color=always --zone-name "$file" "$temp_file"
        fi
    done
    # modified files
    git diff --diff-filter=M --name-only "$oldrev" "$newrev" | while read -r file; do
        if [ "$file" = "_keys" ]; then
            # exclude the _keys zone
            continue
        else
            echo "checking zone $file"
            git show "$oldrev":"$file" > "$old_temp_file"
            git show "$newrev":"$file" > "$temp_file"
            ozone --color=always --old "$old_temp_file" --zone-name "$file" "$temp_file"
        fi
    done
done
