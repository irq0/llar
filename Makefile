ibmplex_tmp := $(shell mktemp --suffix .zip)
fontawesome_tmp := $(shell mktemp --suffix .zip)
bootstrap_tmp := $(shell mktemp --suffix .zip)
waypoints_tmp := $(shell mktemp --suffix .zip)

ibmplex:
	mkdir -p resources/status/ibmplex
	wget -O $(ibmplex_tmp) "https://github.com/IBM/plex/releases/download/v5.1.3/Web.zip"
	unzip -o $(ibmplex_tmp) -d resources/status/ibmplex

fontawesome:
	mkdir -p resources/status/fontawesome
	wget -O $(fontawesome_tmp) "https://use.fontawesome.com/releases/v5.15.3/fontawesome-free-5.15.3-web.zip"
	unzip -o $(fontawesome_tmp) -d resources/status/fontawesome

bootstrap:
	mkdir -p resources/status/bootstrap
	wget -O $(bootstrap_tmp) "https://github.com/twbs/bootstrap/releases/download/v4.6.0/bootstrap-4.6.0-dist.zip"
	unzip -o $(bootstrap_tmp) -d resources/status/bootstrap

jquery:
	mkdir -p resources/status/jquery
	wget -O resources/status/jquery/jquery-3.6.0.min.js "https://code.jquery.com/jquery-3.6.0.min.js"

hammer-js:
	mkdir -p resources/status/hammer
	wget -O resources/status/hammer/hammer.min.js "https://hammerjs.github.io/dist/hammer.min.js"
	wget -O resources/status/hammer/jquery.hammer.js "https://raw.githubusercontent.com/hammerjs/jquery.hammer.js/master/jquery.hammer.js"

waypoints:
	mkdir -p resources/status/waypoints
	wget -O $(waypoints_tmp) "https://github.com/imakewebthings/waypoints/zipball/latest"
	unzip -o $(waypoints_tmp) -d resources/status/waypoints

popper:
	mkdir -p resources/status/popper
	wget -O resources/status/popper/popper.min.js "https://unpkg.com/@popperjs/core@2"

datatables:
	mkdir -p resources/status/datatables
	wget -O resources/status/datatables/jquery.dataTables.min.css "https://cdn.datatables.net/1.10.25/css/jquery.dataTables.min.css"
	wget -O resources/status/datatables/jquery.dataTables.min.js "https://cdn.datatables.net/1.10.25/js/jquery.dataTables.min.js"

leaflet:
	mkdir -p resources/status/leaflet
	wget -O resources/status/leaflet/leaflet.js "https://unpkg.com/leaflet@1.7.1/dist/leaflet.js"
	wget -O resources/status/leaflet/leaflet.css "https://unpkg.com/leaflet@1.7.1/dist/leaflet.css"

web-3rd-party: leaflet datatables popper annotator waypoints hammer-js jquery bootstrap fontawesome ibmplex


uberjar:
	lein uberjar

docker-image: uberjar
	docker build --network host -t irq0/1f596 .
