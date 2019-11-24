run-test:
	echo "Test Suite"
	docker-compose --f docker-compose-test.yml up

run-dev:
	docker-compose --f docker-compose-dev.yml up

ghci-dev:
	docker run -e motoEnv="dev" -i -t  -v "$$HOME/.root:/root" -v "$$PWD:/app" haskell:latest stack ghci --ghc-options="-Wall" --ghci-options="-ghci-script .ghci"

build:
	docker build -f Dockerfile .