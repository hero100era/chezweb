#!/bin/bash
# run.sh - Run the Scheme Flask application

set -e

# Set library path
export CHEZSCHEMELIBDIRS="."

# Check if libuv is installed
if ! ldconfig -p | grep -q libuv; then
    echo "Warning: libuv not found. Please install it:"
    echo "  Ubuntu/Debian: sudo apt-get install libuv1"
    echo "  macOS: brew install libuv"
    echo "  Fedora: sudo dnf install libuv-devel"
fi

# Compile if needed
echo "Compiling libraries..."
scheme --compile-imported-libraries --libdirs "." -q << 'EOF'
(import (chezweb))
EOF

# Run the application
echo "Starting server..."
exec scheme --script examples/app.ss
