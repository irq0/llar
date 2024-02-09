ibmplex_version := 6.3.0
fontawesome_version := 6.5.1
bootstrap_version := 5.3.2
jquery_version := 3.7.1
jquery_datatables_version := 1.13.7

CLOJURE_FILES := $(wildcard *.clj src/**/*.clj test/**/*.clj)

all: web-3rd-party uberjar

.PHONY: all web-3rd-party clean-web-3rd-party ibmplex fontawesome bootstrap jquery hammer-js waypoints popper datatables uberjar docker-image

resources/status/ibmplex/Web/LICENSE.txt:
	mkdir -p resources/status/ibmplex
	wget --quiet -nc -O - "https://github.com/IBM/plex/releases/download/v$(ibmplex_version)/Web.zip" \
	| bsdtar -xzf- -C resources/status/ibmplex
ibmplex: resources/status/ibmplex/Web/LICENSE.txt

resources/status/fontawesome/LICENSE.txt:
	mkdir -p resources/status/fontawesome
	wget --quiet -nc -O - "https://github.com/FortAwesome/Font-Awesome/releases/download/$(fontawesome_version)/fontawesome-free-$(fontawesome_version)-web.zip" \
	| bsdtar -xzf- -C resources/status/fontawesome --strip-components 1 "fontawesome-free-$(fontawesome_version)-web/"
fontawesome: resources/status/fontawesome/LICENSE.txt

resources/status/bootstrap/js/bootstrap.bundle.min.js:
	mkdir -p resources/status/bootstrap
	wget --quiet -nc -O - "https://github.com/twbs/bootstrap/releases/download/v$(bootstrap_version)/bootstrap-$(bootstrap_version)-dist.zip" \
	| bsdtar -xzf- -C resources/status/bootstrap --strip-components 1 "bootstrap-$(bootstrap_version)-dist/"
bootstrap: resources/status/bootstrap/js/bootstrap.bundle.min.js

resources/status/jquery/jquery.min.js:
	mkdir -p resources/status/jquery
	wget --quiet -nc -O resources/status/jquery/jquery.min.js "https://code.jquery.com/jquery-$(jquery_version).min.js"
jquery: resources/status/jquery/jquery.min.js

resources/status/hammer/hammer.min.js:
	mkdir -p resources/status/hammer
	wget --quiet -nc -O resources/status/hammer/hammer.min.js "https://hammerjs.github.io/dist/hammer.min.js"
	wget --quiet -nc -O resources/status/hammer/jquery.hammer.js "https://raw.githubusercontent.com/hammerjs/jquery.hammer.js/master/jquery.hammer.js"
hammer-js: resources/status/hammer/hammer.min.js

resources/status/waypoints/jquery.waypoints.min.js:
	mkdir -p resources/status/waypoints
	wget --quiet -nc -O - "https://github.com/imakewebthings/waypoints/zipball/latest" \
	| bsdtar -xzf- -C resources/status/waypoints --strip-components 2 "imakewebthings-waypoints-*/lib"
waypoints: resources/status/waypoints/jquery.waypoints.min.js

resources/status/popper/popper.min.js:
	mkdir -p resources/status/popper
	wget --quiet -nc -O resources/status/popper/popper.min.js "https://unpkg.com/@popperjs/core@2"
popper: resources/status/popper/popper.min.js

resources/status/datatables/jquery.dataTables.min.css:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/jquery.dataTables.min.css "https://cdn.datatables.net/$(jquery_datatables_version)/css/jquery.dataTables.min.css"
	wget --quiet -nc -O resources/status/datatables/jquery.dataTables.min.js "https://cdn.datatables.net/$(jquery_datatables_version)/js/jquery.dataTables.min.js"
datatables: resources/status/datatables/jquery.dataTables.min.css

web-3rd-party: datatables popper waypoints hammer-js jquery bootstrap fontawesome ibmplex

clean-web-3rd-party:
	rm -rf resources/status/datatables
	rm -rf resources/status/popper
	rm -rf resources/status/waypoints
	rm -rf resources/status/hammer
	rm -rf resources/status/jquery
	rm -rf resources/status/bootstrap
	rm -rf resources/status/fontawesome
	rm -rf resources/status/ibmplex

target/llar-0.1.0-SNAPSHOT-standalone.jar: $(CLOJURE_FILES)
	@echo "Building uberjar"
	@echo "Clojure files: $(CLOJURE_FILES)"
	lein uberjar

uberjar: target/llar-0.1.0-SNAPSHOT-standalone.jar

docker-image: uberjar docker/Dockerfile
	docker build --network host -t ghcr.io/irq0/llar:latest -f docker/Dockerfile .
