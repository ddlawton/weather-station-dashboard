# ==============================================================================
# Makefile for Weather Station Dashboard
# Provides convenient commands for local Docker development and testing
# ==============================================================================

# Variables
IMAGE_NAME = weather-station-dashboard
CONTAINER_NAME = weather-dashboard
PORT = 3838
ENV_FILE = .env

# Colors for output
BLUE = \033[0;34m
GREEN = \033[0;32m
YELLOW = \033[1;33m
NC = \033[0m # No Color

.PHONY: help build run stop restart logs clean shell test status env

# Default target
help:
	@echo "$(BLUE)Weather Station Dashboard - Docker Commands$(NC)"
	@echo ""
	@echo "$(GREEN)Available commands:$(NC)"
	@echo "  make build         - Build Docker image"
	@echo "  make run           - Run container in detached mode"
	@echo "  make run-fg        - Run container in foreground (see logs directly)"
	@echo "  make stop          - Stop running container"
	@echo "  make restart       - Restart container"
	@echo "  make logs          - Show container logs (follow mode)"
	@echo "  make logs-tail     - Show last 100 lines of logs"
	@echo "  make shell         - Open bash shell in running container"
	@echo "  make status        - Show container status"
	@echo "  make clean         - Stop and remove container"
	@echo "  make clean-all     - Stop container, remove image and volumes"
	@echo "  make rebuild       - Clean and rebuild from scratch"
	@echo "  make env           - Create example .env file"
	@echo "  make test          - Build and run for testing"
	@echo ""
	@echo "$(YELLOW)Usage:$(NC)"
	@echo "  1. Create .env file with your DB_PASSWORD"
	@echo "  2. Run 'make build' to build the image"
	@echo "  3. Run 'make run' to start the dashboard"
	@echo "  4. Access at http://localhost:$(PORT)"
	@echo ""

# Build Docker image
build:
	@echo "$(BLUE)Building Docker image...$(NC)"
	docker build -t $(IMAGE_NAME):latest .
	@echo "$(GREEN)Build complete!$(NC)"

# Run container in background
run:
	@echo "$(BLUE)Starting container...$(NC)"
	@if [ -f $(ENV_FILE) ]; then \
		docker run -d \
			--name $(CONTAINER_NAME) \
			--env-file $(ENV_FILE) \
			-p $(PORT):3838 \
			--restart unless-stopped \
			$(IMAGE_NAME):latest; \
	else \
		echo "$(YELLOW)Warning: .env file not found, running without environment variables$(NC)"; \
		docker run -d \
			--name $(CONTAINER_NAME) \
			-p $(PORT):3838 \
			--restart unless-stopped \
			$(IMAGE_NAME):latest; \
	fi
	@echo "$(GREEN)Container started!$(NC)"
	@echo "Access dashboard at http://localhost:$(PORT)"

# Run container in foreground
run-fg:
	@echo "$(BLUE)Starting container in foreground...$(NC)"
	@if [ -f $(ENV_FILE) ]; then \
		docker run --rm \
			--name $(CONTAINER_NAME) \
			--env-file $(ENV_FILE) \
			-p $(PORT):3838 \
			$(IMAGE_NAME):latest; \
	else \
		echo "$(YELLOW)Warning: .env file not found, running without environment variables$(NC)"; \
		docker run --rm \
			--name $(CONTAINER_NAME) \
			-p $(PORT):3838 \
			$(IMAGE_NAME):latest; \
	fi

# Stop container
stop:
	@echo "$(BLUE)Stopping container...$(NC)"
	@docker stop $(CONTAINER_NAME) 2>/dev/null || echo "Container not running"
	@echo "$(GREEN)Container stopped$(NC)"

# Restart container
restart: stop run

# View logs
logs:
	@echo "$(BLUE)Showing container logs (Ctrl+C to exit)...$(NC)"
	docker logs -f $(CONTAINER_NAME)

# View last 100 lines of logs
logs-tail:
	@docker logs --tail 100 $(CONTAINER_NAME)

# Open shell in running container
shell:
	@echo "$(BLUE)Opening shell in container...$(NC)"
	docker exec -it $(CONTAINER_NAME) /bin/bash

# Show container status
status:
	@echo "$(BLUE)Container status:$(NC)"
	@docker ps -a --filter "name=$(CONTAINER_NAME)" --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}" || echo "No container found"
	@echo ""
	@echo "$(BLUE)Image info:$(NC)"
	@docker images $(IMAGE_NAME) --format "table {{.Repository}}\t{{.Tag}}\t{{.Size}}\t{{.CreatedAt}}" || echo "No image found"

# Stop and remove container
clean: stop
	@echo "$(BLUE)Removing container...$(NC)"
	@docker rm $(CONTAINER_NAME) 2>/dev/null || echo "Container already removed"
	@echo "$(GREEN)Cleanup complete$(NC)"

# Remove everything (container, image, volumes)
clean-all: clean
	@echo "$(BLUE)Removing Docker image...$(NC)"
	@docker rmi $(IMAGE_NAME):latest 2>/dev/null || echo "Image already removed"
	@echo "$(GREEN)Full cleanup complete$(NC)"

# Rebuild from scratch
rebuild: clean-all build
	@echo "$(GREEN)Rebuild complete!$(NC)"

# Create example .env file
env:
	@if [ -f $(ENV_FILE) ]; then \
		echo "$(YELLOW).env file already exists, skipping...$(NC)"; \
	else \
		echo "$(BLUE)Creating example .env file...$(NC)"; \
		echo "# Weather Station Dashboard Environment Variables" > $(ENV_FILE); \
		echo "# Copy this file and update with your actual values" >> $(ENV_FILE); \
		echo "" >> $(ENV_FILE); \
		echo "# Database password (required)" >> $(ENV_FILE); \
		echo "DB_PASSWORD=your_password_here" >> $(ENV_FILE); \
		echo "" >> $(ENV_FILE); \
		echo "# Optional: Override database configuration" >> $(ENV_FILE); \
		echo "#DB_HOST=192.168.50.134" >> $(ENV_FILE); \
		echo "#DB_PORT=5432" >> $(ENV_FILE); \
		echo "#DB_NAME=weatherdata" >> $(ENV_FILE); \
		echo "#DB_USER=dlawton" >> $(ENV_FILE); \
		echo "" >> $(ENV_FILE); \
		echo "$(GREEN).env file created! Edit it with your credentials.$(NC)"; \
	fi

# Quick test: build and run
test: build run
	@echo "$(GREEN)Dashboard is starting...$(NC)"
	@echo "Waiting for container to be ready..."
	@sleep 5
	@make logs-tail
	@echo ""
	@echo "$(GREEN)Test complete! Check logs above.$(NC)"
	@echo "Access dashboard at http://localhost:$(PORT)"
	@echo "Run 'make logs' to follow logs in real-time"
