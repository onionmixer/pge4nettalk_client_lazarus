#!/bin/bash
#
# Build script for pge4nettalk_client_lazarus
# Supports both release and debug builds
#

PROJECT_NAME="pge4nettalk_client_lazarus"
FPC="fpc"
BUILD_DIR="build"
DEBUG_DIR="$BUILD_DIR/debug"
RELEASE_DIR="$BUILD_DIR/release"

# Unit paths
UNIT_PATHS="ozapi_pascal:fundamentals/Utils:lazrichview"

# Common compiler options
COMMON_OPTS="-MObjFPC -Scghi -O1 -g -gl -l -vewnhibq -dLCL -dLCLgtk2"
COMMON_OPTS="$COMMON_OPTS -Fu$UNIT_PATHS"
COMMON_OPTS="$COMMON_OPTS -Fi."
COMMON_OPTS="$COMMON_OPTS -dUseCThreads"

# Release build options
RELEASE_OPTS="$COMMON_OPTS -O3 -Xs -XX"

# Debug build options (with DEBUG define and core dump support)
DEBUG_OPTS="$COMMON_OPTS -gw3 -godwarfsets -Criot -dDEBUG"

function show_help {
    echo "Usage: $0 [release|debug|clean|help]"
    echo ""
    echo "Commands:"
    echo "  release   - Build release version (default)"
    echo "  debug     - Build debug version with DEBUG define and core dump support"
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

    $FPC $RELEASE_OPTS \
        -FU"$RELEASE_DIR/units" \
        -FE"$RELEASE_DIR" \
        "$PROJECT_NAME.lpr"

    if [ $? -eq 0 ]; then
        echo "Release build complete: $RELEASE_DIR/$PROJECT_NAME"
    else
        echo "Release build failed!"
        exit 1
    fi
}

function build_debug {
    echo "Building debug version with DEBUG define and core dump support..."
    mkdir -p "$DEBUG_DIR"

    $FPC $DEBUG_OPTS \
        -FU"$DEBUG_DIR/units" \
        -FE"$DEBUG_DIR" \
        "$PROJECT_NAME.lpr"

    if [ $? -eq 0 ]; then
        echo "Debug build complete: $DEBUG_DIR/$PROJECT_NAME"
        echo ""
        echo "Core dumps enabled in binary."
        echo "To allow core dumps, run: ulimit -c unlimited"
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
