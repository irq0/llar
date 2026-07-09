LLAR_VERSION := $(shell lein pprint --no-pretty -- :version)
CLOJURE_FILES := $(wildcard *.clj src/**/*.clj test/**/*.clj)
SHELL=/bin/bash -o pipefail

ibmplex_version := 6.4.0
fontawesome_version := 7.3.0
bootstrap_version := 5.3.8
jquery_version := 4.0.0
chartjs_version := 4.4.8
jquery_datatables_version := 2.1.8
datatables_buttons_version := 3.2.0
llar_uberjar := target/uberjar/llar-$(LLAR_VERSION)-standalone.jar
DOCS_OUT ?= target/docs

all: web-3rd-party uberjar

.PHONY: all web-3rd-party clean-web-3rd-party check-frontend-deps docs-requirements docs-assets ibmplex fontawesome bootstrap jquery hammer-js popper datatables chartjs uberjar docker-image

resources/status/ibmplex/Web/LICENSE.txt:
	mkdir -p resources/status/ibmplex
	wget --quiet -nc -O - "https://github.com/IBM/plex/releases/download/v$(ibmplex_version)/Web.zip" \
	| bsdtar -xzf- -C resources/status/ibmplex
ibmplex: resources/status/ibmplex/Web/LICENSE.txt
resources/status/ibmplex/Web/css/ibm-plex.min.css: resources/status/ibmplex/Web/LICENSE.txt

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

resources/status/hammer/hammer.min.js:
	mkdir -p resources/status/hammer
	wget --quiet -nc -O resources/status/hammer/hammer.min.js "https://hammerjs.github.io/dist/hammer.min.js"
resources/status/hammer/jquery.hammer.js:
	mkdir -p resources/status/hammer
	wget --quiet -nc -O resources/status/hammer/jquery.hammer.js "https://raw.githubusercontent.com/hammerjs/jquery.hammer.js/master/jquery.hammer.js"
hammer-js: resources/status/hammer/hammer.min.js resources/status/hammer/jquery.hammer.js

resources/status/popper/popper.min.js:
	mkdir -p resources/status/popper
	wget --quiet -nc -O resources/status/popper/popper.min.js "https://unpkg.com/@popperjs/core@2"
popper: resources/status/popper/popper.min.js

resources/status/datatables/dataTables.bootstrap5.min.css:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/dataTables.bootstrap5.min.css "https://cdn.datatables.net/$(jquery_datatables_version)/css/dataTables.bootstrap5.min.css"
resources/status/datatables/dataTables.min.js:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/dataTables.min.js "https://cdn.datatables.net/$(jquery_datatables_version)/js/dataTables.min.js"
resources/status/datatables/dataTables.bootstrap5.min.js:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/dataTables.bootstrap5.min.js "https://cdn.datatables.net/$(jquery_datatables_version)/js/dataTables.bootstrap5.min.js"
resources/status/datatables/buttons.bootstrap5.min.css:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/buttons.bootstrap5.min.css "https://cdn.datatables.net/buttons/$(datatables_buttons_version)/css/buttons.bootstrap5.min.css"
resources/status/datatables/dataTables.buttons.min.js:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/dataTables.buttons.min.js "https://cdn.datatables.net/buttons/$(datatables_buttons_version)/js/dataTables.buttons.min.js"
resources/status/datatables/buttons.bootstrap5.min.js:
	mkdir -p resources/status/datatables
	wget --quiet -nc -O resources/status/datatables/buttons.bootstrap5.min.js "https://cdn.datatables.net/buttons/$(datatables_buttons_version)/js/buttons.bootstrap5.min.js"

datatables: resources/status/datatables/dataTables.bootstrap5.min.css resources/status/datatables/dataTables.min.js resources/status/datatables/dataTables.bootstrap5.min.js resources/status/datatables/buttons.bootstrap5.min.css resources/status/datatables/dataTables.buttons.min.js resources/status/datatables/buttons.bootstrap5.min.js

resources/status/chartjs/chart.umd.min.js:
	mkdir -p resources/status/chartjs
	wget --quiet -nc -O resources/status/chartjs/chart.umd.min.js "https://cdn.jsdelivr.net/npm/chart.js@$(chartjs_version)/dist/chart.umd.min.js"
chartjs: resources/status/chartjs/chart.umd.min.js


web-3rd-party: datatables popper hammer-js jquery bootstrap fontawesome ibmplex chartjs

docs-requirements: resources/status/bootstrap/css/bootstrap.min.css resources/status/ibmplex/Web/css/ibm-plex.min.css

docs-assets: docs-requirements resources/status/llar.css
	mkdir -p "$(DOCS_OUT)/static/bootstrap/css" "$(DOCS_OUT)/static/ibmplex/Web/css" "$(DOCS_OUT)/static"
	cp resources/status/bootstrap/css/bootstrap.min.css "$(DOCS_OUT)/static/bootstrap/css/bootstrap.min.css"
	cp resources/status/ibmplex/Web/css/ibm-plex.min.css "$(DOCS_OUT)/static/ibmplex/Web/css/ibm-plex.min.css"
	cp resources/status/llar.css "$(DOCS_OUT)/static/llar.css"

clean-web-3rd-party:
	rm -rf resources/status/datatables
	rm -rf resources/status/popper
	rm -rf resources/status/hammer
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
	results="$$(jq -n '[]')"; \
	for dep in \
		"bootstrap $(bootstrap_version) twbs/bootstrap release" \
		"jquery $(jquery_version) jquery/jquery release" \
		"fontawesome $(fontawesome_version) FortAwesome/Font-Awesome release" \
		"ibmplex $(ibmplex_version) IBM/plex tag" \
		"jquery_datatables $(jquery_datatables_version) DataTables/DataTablesSrc release"; \
	do \
		set -- $$dep; \
		name="$$1"; \
		current="$$2"; \
		repo="$$3"; \
		method="$$4"; \
		if [ "$$method" = "tag" ]; then \
			latest="$$(get_latest_semver_tag "$$repo" || echo unknown)"; \
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
