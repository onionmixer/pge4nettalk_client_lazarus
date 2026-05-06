#!/bin/bash
#
# Build script for pge4nettalk_client_lazarus
# Builds the Lazarus project and copies the binary to a stable output path.
#

PROJECT_NAME="pge4nettalk_client_lazarus"
LAZBUILD="${LAZBUILD:-lazbuild}"
BUILD_DIR="build"
DEBUG_DIR="$BUILD_DIR/debug"
RELEASE_DIR="$BUILD_DIR/release"

function show_help {
    echo "Usage: $0 [release|debug|clean|help]"
    echo ""
    echo "Commands:"
    echo "  release   - Build release version (default)"
    echo "  debug     - Force rebuild using the Lazarus project settings"
    echo "  clean     - Clean build artifacts"
    echo "  help      - Show this help message"
    echo ""
    echo "Output locations:"
    echo "  Release: $RELEASE_DIR/$PROJECT_NAME"
    echo "  Debug:   $DEBUG_DIR/$PROJECT_NAME"
}

function build_release {
    echo "Building release version..."
    mkdir -p "$RELEASE_DIR"

    $LAZBUILD "$PROJECT_NAME.lpi"

    if [ $? -eq 0 ]; then
        cp -f "$PROJECT_NAME" "$RELEASE_DIR/$PROJECT_NAME"
        echo "Release build complete: $RELEASE_DIR/$PROJECT_NAME"
    else
        echo "Release build failed!"
        exit 1
    fi
}

function build_debug {
    echo "Building debug version..."
    mkdir -p "$DEBUG_DIR"

    $LAZBUILD -B "$PROJECT_NAME.lpi"

    if [ $? -eq 0 ]; then
        cp -f "$PROJECT_NAME" "$DEBUG_DIR/$PROJECT_NAME"
        echo "Debug build complete: $DEBUG_DIR/$PROJECT_NAME"
    else
        echo "Debug build failed!"
        exit 1
    fi
}

function clean_build {
    echo "Cleaning build artifacts..."
    rm -rf "$BUILD_DIR"
    rm -rf lib
    rm -f "$PROJECT_NAME"
    find . -name "*.o" -delete
    find . -name "*.ppu" -delete
    find . -name "*.compiled" -delete
    echo "Clean complete"
}

# Main
case "${1:-release}" in
    release)
        build_release
        ;;
    debug)
        build_debug
        ;;
    clean)
        clean_build
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        echo "Unknown command: $1"
        show_help
        exit 1
        ;;
esac
