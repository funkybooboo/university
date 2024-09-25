#!/bin/bash

# Build the project
./gradlew build

# Check if the build was successful
if [ $? -eq 0 ]; then
    # Run the application
    java -jar build/libs/Assign3.jar
else
    echo "Build failed. Please check the errors above."
    exit 1
fi

