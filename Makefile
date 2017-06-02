.PHONY: package-linux package-macos publish clean elm-build

DOCKER_LINUX_IMAGE="fpco/stack-build:lts-8.5"
API_HOST=https://api.github.com
UPLOAD_HOST=https://uploads.github.com
DASH_VERSION=$(shell echo $(VERSION) | sed -e s/\\./-/g)

ifdef GITHUB_TOKEN
	AUTH=-H 'Authorization: token $(GITHUB_TOKEN)'
endif


# Utility target for checking required parameters
guard-%:
	@if [ "$($*)" = '' ]; then \
		echo "Missing required $* variable."; \
		exit 1; \
	fi;

pull-linux-image:
	docker pull $(DOCKER_LINUX_IMAGE)

elm-build:
	elm-package install --yes
	elm-make UI/UI.elm --output assets/app.js

package-linux: pull-linux-image elm-build wrecker-ui-linux/wrecker-ui
	cp -a assets wrecker-ui-linux
	zip -r wrecker-ui-linux.zip wrecker-ui-linux

package-macos: elm-build wrecker-ui-macos/wrecker-ui
	cp -a assets wrecker-ui-macos
	zip -r wrecker-ui-macos.zip wrecker-ui-macos

wrecker-ui-linux/wrecker-ui:
	mkdir -p wrecker-ui-linux
	stack --docker --docker-image $(DOCKER_LINUX_IMAGE) install --local-bin-path wrecker-ui-linux

wrecker-ui-macos/wrecker-ui:
	mkdir -p wrecker-ui-macos
	stack install --local-bin-path wrecker-ui-macos

release.json: package-linux package-macos
	@echo "Creating draft release for $(VERSION)"
	@curl $(AUTH) -XPOST $(API_HOST)/repos/seatgeek/wrecker-ui/releases -d '{ \
		"tag_name": "$(VERSION)", \
		"name": "Wrecker UI $(VERSION)", \
		"draft": false, \
		"prerelease": false \
	}' > release.json
	@echo "Uploading zip file to github."

publish: guard-VERSION guard-GITHUB_TOKEN release.json
	$(eval RELEASE_ID := $(shell cat release.json | jq .id))
	@sleep 1
	@echo "Uploading the Linux wrecker-ui"
	@curl $(AUTH) -XPOST \
		$(UPLOAD_HOST)/repos/seatgeek/wrecker-ui/releases/$(RELEASE_ID)/assets?name=wrecker-ui-linux.zip \
		-H "Accept: application/vnd.github.manifold-preview" \
		-H 'Content-Type: application/octet-stream' \
		--data-binary '@wrecker-ui-linux.zip' > /dev/null
	@echo "Uploading the MacOS binary"
	@curl $(AUTH) -XPOST \
		$(UPLOAD_HOST)/repos/seatgeek/wrecker-ui/releases/$(RELEASE_ID)/assets?name=wrecker-ui-macos.zip \
		-H "Accept: application/vnd.github.manifold-preview" \
		-H 'Content-Type: application/octet-stream' \
		--data-binary '@wrecker-ui-macos.zip' > /dev/null
	@echo Release done, you can go to:
	@cat release.json | jq .html_url


clean:
	rm -rf wrecker-ui-*
	rm -f release.json
