#!/bin/bash

# Kaede Docker environment build and run script

set -e

IMAGE_NAME="kaede-dev"
CONTAINER_NAME="kaede-container"

# Function definitions
build_image() {
    echo "🔨 Building Kaede Docker image..."
    docker build -t $IMAGE_NAME .
    echo "✅ Image build completed"
}

run_container() {
    echo "🚀 Starting Kaede container..."
    
    # Remove existing container if present
    if docker ps -a --format 'table {{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        echo "Removing existing container..."
        docker rm -f $CONTAINER_NAME
    fi
    
    # Mount current directory as workspace
    docker run -it \
        --name $CONTAINER_NAME \
        -v "$(pwd):/workspace" \
        -w /workspace \
        $IMAGE_NAME \
        /bin/bash
}

test_installation() {
    echo "🧪 Testing Kaede installation..."
    docker run --rm $IMAGE_NAME kaede --help
    echo "✅ Kaede is successfully installed"
}

show_help() {
    echo "Kaede Docker Environment Management Script"
    echo ""
    echo "Usage:"
    echo "  $0 [command]"
    echo ""
    echo "Commands:"
    echo "  build    - Build Docker image"
    echo "  run      - Start container (interactive mode)"
    echo "  test     - Test installation"
    echo "  clean    - Remove image and container"
    echo "  help     - Show this help"
    echo ""
    echo "Examples:"
    echo "  $0 build    # Build image"
    echo "  $0 run      # Start container"
    echo "  $0 test     # Run tests"
}

clean_all() {
    echo "🧹 Removing Docker images and containers..."
    
    # Remove container
    if docker ps -a --format 'table {{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        docker rm -f $CONTAINER_NAME
        echo "Container removed"
    fi
    
    # Remove image
    if docker images --format 'table {{.Repository}}' | grep -q "^${IMAGE_NAME}$"; then
        docker rmi $IMAGE_NAME
        echo "Image removed"
    fi
    
    echo "✅ Cleanup completed"
}

# Main processing
case "${1:-help}" in
    build)
        build_image
        ;;
    run)
        # Auto-build if image doesn't exist
        if ! docker images --format 'table {{.Repository}}' | grep -q "^${IMAGE_NAME}$"; then
            echo "Image not found. Building automatically..."
            build_image
        fi
        run_container
        ;;
    test)
        test_installation
        ;;
    clean)
        clean_all
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        echo "❌ Invalid command: $1"
        echo ""
        show_help
        exit 1
        ;;
esac
