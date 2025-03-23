#!/bin/bash

# Source directory
src_dir="main/kotlin"

# Destination directory
dest_dir="test/kotlin"

# Find all Kotlin files in source directory
find "$src_dir" -type f -name "*.kt" | while read file; do
    # Extract directory and filename from the full path
    dir=$(dirname "$file")
    filename=$(basename "$file")
    # Create corresponding directory in the destination if it doesn't exist
    dest_subdir="$dest_dir/${dir#$src_dir/}"
    mkdir -p "$dest_subdir"
    # Touch a new file in the destination with "Test" appended to the filename
    touch "$dest_subdir/${filename%.kt}Test.kt"
done