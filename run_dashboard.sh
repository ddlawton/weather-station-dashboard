#!/bin/bash
# ==============================================================================
# run_dashboard.sh
# Standalone script to manage Weather Station Dashboard on TrueNAS Scale
# This script provides all necessary Docker commands without requiring Make
# ==============================================================================

set -e  # Exit on error

# Configuration
IMAGE_NAME="weather-station-dashboard"
CONTAINER_NAME="weather-dashboard"
PORT="3838"
ENV_FILE=".env"

# Colors for output
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# ==============================================================================
# Helper Functions
# ==============================================================================

print_header() {
    echo -e "${BLUE}================================================${NC}"
    echo -e "${BLUE}Weather Station Dashboard Manager${NC}"
    echo -e "${BLUE}================================================${NC}"
    echo ""
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# ==============================================================================
# Command Functions
# ==============================================================================

show_help() {
    print_header
    echo "Usage: ./run_dashboard.sh [COMMAND]"
    echo ""
    echo "Commands:"
    echo "  build         - Build Docker image from Dockerfile"
    echo "  start         - Start the dashboard container"
    echo "  stop          - Stop the dashboard container"
    echo "  restart       - Restart the dashboard container"
    echo "  status        - Show container and image status"
    echo "  logs          - Show container logs (last 100 lines)"
    echo "  logs-follow   - Follow container logs in real-time"
    echo "  shell         - Open bash shell in running container"
    echo "  update        - Pull latest code and rebuild"
    echo "  clean         - Stop and remove container"
    echo "  clean-all     - Remove container and image"
    echo "  setup-env     - Create example .env file"
    echo ""
    echo "Examples:"
    echo "  ./run_dashboard.sh build        # Build the image"
    echo "  ./run_dashboard.sh start        # Start the dashboard"
    echo "  ./run_dashboard.sh logs-follow  # Watch logs"
    echo "  ./run_dashboard.sh restart      # Restart after config change"
    echo ""
    echo "First time setup:"
    echo "  1. ./run_dashboard.sh setup-env"
    echo "  2. Edit .env file with your database password"
    echo "  3. ./run_dashboard.sh build"
    echo "  4. ./run_dashboard.sh start"
    echo ""
}

build_image() {
    print_info "Building Docker image: ${IMAGE_NAME}..."
    
    if docker build -t "${IMAGE_NAME}:latest" .; then
        print_success "Image built successfully!"
        docker images "${IMAGE_NAME}"
    else
        print_error "Build failed!"
        exit 1
    fi
}

start_container() {
    print_info "Starting container: ${CONTAINER_NAME}..."
    
    # Check if container already exists
    if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        print_warning "Container already exists. Use 'restart' or 'clean' first."
        
        # Check if it's just stopped
        if ! docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
            print_info "Container is stopped. Starting it..."
            docker start "${CONTAINER_NAME}"
            print_success "Container started!"
            show_access_info
            return 0
        else
            print_warning "Container is already running."
            show_access_info
            return 0
        fi
    fi
    
    # Start new container
    if [ -f "${ENV_FILE}" ]; then
        print_info "Using environment file: ${ENV_FILE}"
        docker run -d \
            --name "${CONTAINER_NAME}" \
            --env-file "${ENV_FILE}" \
            -p "${PORT}:3838" \
            --restart unless-stopped \
            "${IMAGE_NAME}:latest"
    else
        print_warning "No .env file found. Running without environment variables."
        print_warning "Database password may not be set!"
        docker run -d \
            --name "${CONTAINER_NAME}" \
            -p "${PORT}:3838" \
            --restart unless-stopped \
            "${IMAGE_NAME}:latest"
    fi
    
    if [ $? -eq 0 ]; then
        print_success "Container started successfully!"
        sleep 2
        show_access_info
    else
        print_error "Failed to start container!"
        exit 1
    fi
}

stop_container() {
    print_info "Stopping container: ${CONTAINER_NAME}..."
    
    if docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        docker stop "${CONTAINER_NAME}"
        print_success "Container stopped!"
    else
        print_warning "Container is not running."
    fi
}

restart_container() {
    print_info "Restarting container: ${CONTAINER_NAME}..."
    stop_container
    sleep 2
    start_container
}

show_status() {
    print_header
    print_info "Container Status:"
    echo ""
    
    if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        docker ps -a --filter "name=${CONTAINER_NAME}" \
            --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
    else
        echo "  No container found."
    fi
    
    echo ""
    print_info "Image Status:"
    echo ""
    
    if docker images "${IMAGE_NAME}" --format '{{.Repository}}' | grep -q "^${IMAGE_NAME}$"; then
        docker images "${IMAGE_NAME}" \
            --format "table {{.Repository}}\t{{.Tag}}\t{{.Size}}\t{{.CreatedAt}}"
    else
        echo "  No image found."
    fi
    
    echo ""
}

show_logs() {
    print_info "Showing last 100 lines of logs..."
    echo ""
    docker logs --tail 100 "${CONTAINER_NAME}"
}

follow_logs() {
    print_info "Following logs (Ctrl+C to exit)..."
    echo ""
    docker logs -f "${CONTAINER_NAME}"
}

open_shell() {
    print_info "Opening bash shell in container..."
    print_warning "Type 'exit' to leave the shell"
    echo ""
    docker exec -it "${CONTAINER_NAME}" /bin/bash
}

update_app() {
    print_info "Updating application..."
    
    # Check if we're in a git repo
    if [ -d ".git" ]; then
        print_info "Pulling latest changes..."
        git pull
    else
        print_warning "Not a git repository. Skipping git pull."
    fi
    
    print_info "Stopping old container..."
    stop_container
    
    print_info "Removing old container..."
    docker rm "${CONTAINER_NAME}" 2>/dev/null || true
    
    print_info "Rebuilding image..."
    build_image
    
    print_info "Starting new container..."
    start_container
    
    print_success "Update complete!"
}

clean_container() {
    print_info "Cleaning up container..."
    stop_container
    
    if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
        docker rm "${CONTAINER_NAME}"
        print_success "Container removed!"
    else
        print_warning "No container to remove."
    fi
}

clean_all() {
    print_warning "This will remove both the container AND the image!"
    read -p "Are you sure? (y/N): " -n 1 -r
    echo
    
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        clean_container
        
        if docker images "${IMAGE_NAME}" --format '{{.Repository}}' | grep -q "^${IMAGE_NAME}$"; then
            print_info "Removing image..."
            docker rmi "${IMAGE_NAME}:latest"
            print_success "Image removed!"
        else
            print_warning "No image to remove."
        fi
        
        print_success "Full cleanup complete!"
    else
        print_info "Cleanup cancelled."
    fi
}

setup_env_file() {
    if [ -f "${ENV_FILE}" ]; then
        print_warning ".env file already exists!"
        read -p "Overwrite it? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            print_info "Keeping existing .env file."
            return 0
        fi
    fi
    
    print_info "Creating .env file..."
    
    cat > "${ENV_FILE}" << 'EOF'
# Weather Station Dashboard Environment Variables
# Edit this file with your actual values

# Database password (REQUIRED)
DB_PASSWORD=your_password_here

# Optional: Override database configuration
# Uncomment and modify if needed
#DB_HOST=192.168.50.134
#DB_PORT=5432
#DB_NAME=weatherdata
#DB_USER=dlawton

# Optional: Timezone configuration
#TIMEZONE_DISPLAY=America/New_York
EOF
    
    print_success ".env file created!"
    print_warning "IMPORTANT: Edit .env and set your DB_PASSWORD before starting!"
    echo ""
    print_info "To edit: nano .env  (or use your preferred editor)"
}

show_access_info() {
    echo ""
    print_success "Dashboard is running!"
    echo ""
    print_info "Access URLs:"
    echo "  Local:   http://localhost:${PORT}"
    echo "  Network: http://$(hostname -I | awk '{print $1}'):${PORT}"
    echo ""
    print_info "Useful commands:"
    echo "  View logs:    ./run_dashboard.sh logs-follow"
    echo "  Stop:         ./run_dashboard.sh stop"
    echo "  Restart:      ./run_dashboard.sh restart"
    echo ""
}

# ==============================================================================
# Main Script Logic
# ==============================================================================

# Check if Docker is available
if ! command -v docker &> /dev/null; then
    print_error "Docker is not installed or not in PATH!"
    exit 1
fi

# Parse command
case "${1:-help}" in
    build)
        build_image
        ;;
    start)
        start_container
        ;;
    stop)
        stop_container
        ;;
    restart)
        restart_container
        ;;
    status)
        show_status
        ;;
    logs)
        show_logs
        ;;
    logs-follow)
        follow_logs
        ;;
    shell)
        open_shell
        ;;
    update)
        update_app
        ;;
    clean)
        clean_container
        ;;
    clean-all)
        clean_all
        ;;
    setup-env)
        setup_env_file
        ;;
    help|--help|-h)
        show_help
        ;;
    *)
        print_error "Unknown command: $1"
        echo ""
        show_help
        exit 1
        ;;
esac

exit 0
