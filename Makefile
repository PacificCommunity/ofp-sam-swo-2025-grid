# Docker image and working directory
DOCKER_IMAGE=ghcr.io/pacificcommunity/ss3-3.30.23.1:v1.2
WORKDIR=/workspace


# inside containers
ss3: ScenarioSetup_SWO2025.R
	Rscript ScenarioSetup_SWO2025.R
	
# inside containers
clean: 
	@if [ -d "grids" ]; then sudo rm -rf grids; fi

# Docker image pull target
docker-pull:
	@echo "Pulling Docker image: $(DOCKER_IMAGE)"
	docker pull $(DOCKER_IMAGE)
	
# docker-interactive
docker-interactive:
	docker run -d --name swo-container \
		-v "$(CURDIR):$(WORKDIR)" \
		-w $(WORKDIR) \
		-p 8787:8787 \
		$(DOCKER_IMAGE) tail -f /dev/null
		
docker-ss3: ScenarioSetup_SWO2025.R
	@if [ -d "grids" ]; then sudo chown -R $(whoami) grids && chmod -R u+w grids && rm -rf grids; fi
	docker run --rm -v "$(CURDIR):$(WORKDIR)" -w $(WORKDIR) $(DOCKER_IMAGE) Rscript ScenarioSetup_SWO2025.R

