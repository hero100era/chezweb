#!/bin/bash
# build.sh - Compile all libraries

set -e

export CHEZSCHEMELIBDIRS="."

echo "========================================"
echo "  Building Scheme Flask Framework"
echo "========================================"

# Create directories
mkdir -p compiled

# Compile libraries in order
echo "Compiling (utils helpers)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (utils helpers))
EOF

echo "Compiling (utils json)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (utils json))
EOF

echo "Compiling (utils url)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (utils url))
EOF

echo "Compiling (utils template)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (utils template))
EOF

echo "Compiling (ffi libuv)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (ffi libuv))
EOF

echo "Compiling (core request)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (core request))
EOF

echo "Compiling (core response)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (core response))
EOF

echo "Compiling (core router)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (core router))
EOF

echo "Compiling (core middleware)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (core middleware))
EOF

echo "Compiling (core server)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (core server))
EOF

echo "Compiling (chezweb)..."
scheme --compile-imported-libraries -q << 'EOF'
(import (chezweb))
EOF

echo ""
echo "Build complete!"
echo "Run with: ./run.sh"
