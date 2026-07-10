LLAR_VERSION := $(shell lein pprint --no-pretty -- :version)
CLOJURE_FILES := $(wildcard *.clj src/**/*.clj test/**/*.clj)
SHELL=/bin/bash -o pipefail

ibmplex_sans_version := 1.1.0
ibmplex_sans_condensed_version := 2.0.0
ibmplex_mono_version := 2.5.0
ibmplex_serif_version := 2.0.0
ibmplex_web_dir := resources/status/ibmplex/Web
ibmplex_css := $(ibmplex_web_dir)/css/ibm-plex.min.css
ibmplex_stamp := $(ibmplex_web_dir)/.packages-$(ibmplex_sans_version)-$(ibmplex_sans_condensed_version)-$(ibmplex_mono_version)-$(ibmplex_serif_version).stamp
ibmplex_families := IBM-Plex-Sans IBM-Plex-Sans-Condensed IBM-Plex-Mono IBM-Plex-Serif
ibmplex_packages := \
	"IBM-Plex-Sans @ibm/plex-sans plex-sans $(ibmplex_sans_version) ibm-plex-sans" \
	"IBM-Plex-Sans-Condensed @ibm/plex-sans-condensed plex-sans-condensed $(ibmplex_sans_condensed_version) ibm-plex-sans-condensed" \
	"IBM-Plex-Mono @ibm/plex-mono plex-mono $(ibmplex_mono_version) ibm-plex-mono" \
	"IBM-Plex-Serif @ibm/plex-serif plex-serif $(ibmplex_serif_version) ibm-plex-serif"
fontawesome_version := 7.3.0
bootstrap_version := 5.3.8
jquery_version := 4.0.0
chartjs_version := 4.5.1
datatables_version := 3.0.0-beta.2
llar_uberjar := target/uberjar/llar-$(LLAR_VERSION)-standalone.jar
DOCS_OUT ?= target/docs

all: web-3rd-party uberjar

.PHONY: all web-3rd-party clean-web-3rd-party check-frontend-deps docs-requirements docs-assets ibmplex fontawesome bootstrap jquery datatables chartjs uberjar docker-image

$(ibmplex_stamp):
	@set -e; \
	for package in $(ibmplex_packages); do \
		set -- $$package; \
		dir="$$1"; \
		scope="$$2"; \
		archive="$$3"; \
		version="$$4"; \
		rm -rf "$(ibmplex_web_dir)/$$dir"; \
		mkdir -p "$(ibmplex_web_dir)/$$dir"; \
		wget --quiet -O - "https://registry.npmjs.org/$$scope/-/$$archive-$$version.tgz" \
		| bsdtar -xzf- -C "$(ibmplex_web_dir)/$$dir" --strip-components 1; \
	done; \
	touch "$@"

$(ibmplex_css): $(ibmplex_stamp)
	@set -e; \
	mkdir -p "$(ibmplex_web_dir)/css"; \
	: > "$@"; \
	for package in $(ibmplex_packages); do \
		set -- $$package; \
		dir="$$1"; \
		css="$$5"; \
		sed "s#url(\"../fonts/#url(\"../$$dir/fonts/#g" "$(ibmplex_web_dir)/$$dir/css/$$css-all.min.css" >> "$@"; \
	done
ibmplex: $(ibmplex_css)

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
resources/status/bootstrap/css/bootstrap.min.css: resources/status/bootstrap/js/bootstrap.bundle.min.js

resources/status/jquery/jquery.min.js:
	mkdir -p resources/status/jquery
	wget --quiet -nc -O resources/status/jquery/jquery.min.js "https://code.jquery.com/jquery-$(jquery_version).min.js"
jquery: resources/status/jquery/jquery.min.js

resources/status/datatables/dataTables.bootstrap5.min.css:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/dataTables.bootstrap5.min.css "https://cdn.datatables.net/$(datatables_version)/css/dataTables.bootstrap5.min.css"
resources/status/datatables/dataTables.min.js:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/dataTables.min.js "https://cdn.datatables.net/$(datatables_version)/js/dataTables.min.js"
resources/status/datatables/dataTables.bootstrap5.min.js:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/dataTables.bootstrap5.min.js "https://cdn.datatables.net/$(datatables_version)/js/dataTables.bootstrap5.min.js"

datatables: resources/status/datatables/dataTables.bootstrap5.min.css resources/status/datatables/dataTables.min.js resources/status/datatables/dataTables.bootstrap5.min.js

resources/status/chartjs/chart.umd.min.js:
	mkdir -p resources/status/chartjs
	wget --quiet -nc -O resources/status/chartjs/chart.umd.min.js "https://cdn.jsdelivr.net/npm/chart.js@$(chartjs_version)/dist/chart.umd.min.js"
chartjs: resources/status/chartjs/chart.umd.min.js


web-3rd-party: datatables jquery bootstrap fontawesome ibmplex chartjs

docs-requirements: resources/status/bootstrap/css/bootstrap.min.css resources/status/ibmplex/Web/css/ibm-plex.min.css

docs-assets: docs-requirements resources/status/llar.css
	mkdir -p "$(DOCS_OUT)/static/bootstrap/css" "$(DOCS_OUT)/static/ibmplex/Web/css" "$(DOCS_OUT)/static"
	cp resources/status/bootstrap/css/bootstrap.min.css "$(DOCS_OUT)/static/bootstrap/css/bootstrap.min.css"
	cp resources/status/ibmplex/Web/css/ibm-plex.min.css "$(DOCS_OUT)/static/ibmplex/Web/css/ibm-plex.min.css"
	@for family in $(ibmplex_families); do \
		cp -R "$(ibmplex_web_dir)/$$family" "$(DOCS_OUT)/static/ibmplex/Web/"; \
	done
	cp resources/status/llar.css "$(DOCS_OUT)/static/llar.css"

clean-web-3rd-party:
	rm -rf resources/status/datatables
	rm -rf resources/status/jquery
	rm -rf resources/status/bootstrap
	rm -rf resources/status/fontawesome
	rm -rf resources/status/ibmplex
	rm -rf resources/status/chartjs

check-frontend-deps:
	@set -euo pipefail; \
	get_latest_release() { \
		curl -sf "https://api.github.com/repos/$$1/releases/latest" \
		| jq -r '.tag_name' \
		| sed 's/^v//'; \
	}; \
	get_latest_semver_tag() { \
		curl -sf "https://api.github.com/repos/$$1/tags?per_page=20" \
		| jq -r '.[].name' \
		| grep -E '^v?[0-9]+\.[0-9]+\.[0-9]+$$' \
		| sed 's/^v//' \
		| sort -V \
		| tail -1; \
	}; \
	get_latest_npm() { \
		curl -sf "https://registry.npmjs.org/$$(printf '%s' "$$1" | sed 's#/#%2f#')/latest" \
		| jq -r '.version'; \
	}; \
	results="$$(jq -n '[]')"; \
	for dep in \
		"bootstrap $(bootstrap_version) twbs/bootstrap release" \
		"jquery $(jquery_version) jquery/jquery release" \
		"fontawesome $(fontawesome_version) FortAwesome/Font-Awesome release" \
		"ibmplex_sans $(ibmplex_sans_version) @ibm/plex-sans npm" \
		"ibmplex_sans_condensed $(ibmplex_sans_condensed_version) @ibm/plex-sans-condensed npm" \
		"ibmplex_mono $(ibmplex_mono_version) @ibm/plex-mono npm" \
		"ibmplex_serif $(ibmplex_serif_version) @ibm/plex-serif npm" \
		"chartjs $(chartjs_version) chart.js npm" \
		"datatables $(datatables_version) DataTables/DataTablesSrc release"; \
	do \
		set -- $$dep; \
		name="$$1"; \
		current="$$2"; \
		repo="$$3"; \
		method="$$4"; \
		if [ "$$method" = "tag" ]; then \
			latest="$$(get_latest_semver_tag "$$repo" || echo unknown)"; \
		elif [ "$$method" = "npm" ]; then \
			latest="$$(get_latest_npm "$$repo" || echo unknown)"; \
		else \
			latest="$$(get_latest_release "$$repo" || echo unknown)"; \
		fi; \
		results="$$(echo "$$results" | jq \
			--arg name "$$name" \
			--arg current "$$current" \
			--arg latest "$$latest" \
			--arg repo "$$repo" \
			'. + [{"name": $$name, "current": $$current, "latest": $$latest, "repo": $$repo}]')"; \
	done; \
	echo "$$results" | jq .

$(llar_uberjar): $(CLOJURE_FILES)
	@echo "Building uberjar with version $(LLAR_VERSION)"
	@echo "Clojure files: $(CLOJURE_FILES)"
	lein uberjar

uberjar: $(llar_uberjar)

docker-image: uberjar docker/Dockerfile
	docker build --build-arg VERSION=$(LLAR_VERSION) --network host -t ghcr.io/irq0/llar:latest -f docker/Dockerfile .
