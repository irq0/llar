ibmplex_version := 6.3.0
fontawesome_version := 6.5.1
bootstrap_version := 5.3.2
jquery_version := 3.7.1
jquery_datatables_version := 1.13.7

all: web-3rd-party uberjar

ibmplex:
	mkdir -p resources/status/ibmplex
	wget --quiet -nc -O - "https://github.com/IBM/plex/releases/download/v$(ibmplex_version)/Web.zip" \
	| bsdtar -xzf- -C resources/status/ibmplex

fontawesome:
	mkdir -p resources/status/fontawesome
	wget --quiet -nc -O - "https://github.com/FortAwesome/Font-Awesome/releases/download/$(fontawesome_version)/fontawesome-free-$(fontawesome_version)-web.zip" \
	| bsdtar -xzf- -C resources/status/fontawesome --strip-components 1 "fontawesome-free-$(fontawesome_version)-web/"

bootstrap:
	mkdir -p resources/status/bootstrap
	wget --quiet -nc -O - "https://github.com/twbs/bootstrap/releases/download/v$(bootstrap_version)/bootstrap-$(bootstrap_version)-dist.zip" \
	| bsdtar -xzf- -C resources/status/bootstrap --strip-components 1 "bootstrap-$(bootstrap_version)-dist/"

jquery:
	mkdir -p resources/status/jquery
	wget --quiet -nc -O resources/status/jquery/jquery.min.js "https://code.jquery.com/jquery-$(jquery_version).min.js"

hammer-js:
	mkdir -p resources/status/hammer
	wget --quiet -nc -O resources/status/hammer/hammer.min.js "https://hammerjs.github.io/dist/hammer.min.js"
	wget --quiet -nc -O resources/status/hammer/jquery.hammer.js "https://raw.githubusercontent.com/hammerjs/jquery.hammer.js/master/jquery.hammer.js"

waypoints:
	mkdir -p resources/status/waypoints
	wget --quiet -nc -O - "https://github.com/imakewebthings/waypoints/zipball/latest" \
	| bsdtar -xzf- -C resources/status/waypoints --strip-components 2 "imakewebthings-waypoints-*/lib"

popper:
	mkdir -p resources/status/popper
	wget --quiet -nc -O resources/status/popper/popper.min.js "https://unpkg.com/@popperjs/core@2"

datatables:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/jquery.dataTables.min.css "https://cdn.datatables.net/$(jquery_datatables_version)/css/jquery.dataTables.min.css"
	wget --quiet -nc -O resources/status/datatables/jquery.dataTables.min.js "https://cdn.datatables.net/$(jquery_datatables_version)/js/jquery.dataTables.min.js"

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

uberjar:
	lein uberjar

docker-image: uberjar
	docker build --network host -t irq0/1f596 .
