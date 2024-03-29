KIND_VERSION:=0.20.0
KUBERNETES_VERSION:=1.28.0
HELM_VERSION:=3.14.0
WD:=$(dir $(lastword $(MAKEFILE_LIST)))
BIN:=$(WD)/_bin
KIND:=$(BIN)/kind-$(KIND_VERSION)
KIND_CLUSTER_NAME=waq-test-cluster
KUBECTL:=$(BIN)/kubectl-$(KUBERNETES_VERSION)
HELM:=$(BIN)/helm-$(HELM_VERSION)
IMAGE=waq:dev

.PHONY: build
build:
	dune build

.PHONY: run-test
run-test:
	env \
		KUBECTL=$(KUBECTL) \
		MANIFESTS=$(WD)/manifests \
		OCAMLRUNPARAM=b \
		E2E_TEST_WAQ_SERVER_NAME=https://$(shell cat $(WD)/_test_waq_domain) \
		E2E_TEST_MASTODON_SERVER_NAME=https://$(shell cat $(WD)/_test_mastodon_domain) \
		dune exec src/main.exe
	#env KUBECTL=$(KUBECTL) MANIFESTS=$(WD)/manifests OCAMLRUNPARAM=b E2E_TEST_WAQ_SERVER_NAME=http://localhost:58080 dune exec src/main.exe

.PHONY: test
test:
	$(MAKE) start-waq
	$(MAKE) start-mastodon
	$(MAKE) run-test

$(BIN):
	mkdir -p $(BIN)

$(KIND): | $(BIN)
	wget -O $(KIND) https://github.com/kubernetes-sigs/kind/releases/download/v$(KIND_VERSION)/kind-linux-amd64
	chmod a+x $(KIND)

$(KUBECTL): | $(BIN)
	wget -O $(KUBECTL) https://storage.googleapis.com/kubernetes-release/release/v$(KUBERNETES_VERSION)/bin/linux/amd64/kubectl
	chmod a+x $(KUBECTL)

$(HELM): | $(BIN)
	curl -sSLf https://get.helm.sh/helm-v$(HELM_VERSION)-linux-amd64.tar.gz \
		| tar xvz -C $(BIN) --strip-components 1 linux-amd64/helm
	mv $(BIN)/helm $@

KUBECONFIG := $(shell pwd)/.kubeconfig
.PHONY: $(KUBECONFIG)
$(KUBECONFIG): | $(KIND)
	$(KIND) export kubeconfig --name $(KIND_CLUSTER_NAME) --kubeconfig=$@

.PHONY: setup
setup: $(KIND) $(KUBECTL) $(HELM)

.PHONY: build-image
build-image:
	cd $(WD)/.. && docker build . -t $(IMAGE)

.PHONY: clean-cluster
clean-cluster:
	$(KIND) delete cluster -n $(KIND_CLUSTER_NAME)

.PHONY: start-ngrok
start-ngrok:
	systemctl --user reset-failed ngrok-waq || true
	systemctl --user reset-failed ngrok-mastodon || true
	systemctl --user reset-failed ngrok-elk || true
	systemd-run --user --unit=ngrok-waq --working-directory=$(WD) ./start-ngrok.sh waq 58080
	systemd-run --user --unit=ngrok-mastodon --working-directory=$(WD) ./start-ngrok.sh mastodon 58081
	systemd-run --user --unit=ngrok-elk --working-directory=$(WD) ./start-ngrok.sh elk 58082

.PHONY: stop-ngrok
stop-ngrok:
	systemctl --user stop ngrok-waq || true
	rm -f _test_waq_domain
	systemctl --user stop ngrok-mastodon || true
	rm -f _test_mastodon_domain
	systemctl --user stop ngrok-elk || true
	rm -f _test_elk_domain

.PHONY: create-cluster
create-cluster:
	$(MAKE) setup
	$(MAKE) clean-cluster
	$(KIND) create cluster -n $(KIND_CLUSTER_NAME) --image kindest/node:v$(KUBERNETES_VERSION)
	$(MAKE) $(KUBECONFIG)
	$(KUBECTL) create namespace e2e
	KUBECONFIG=$(KUBECONFIG) $(HELM) install --namespace e2e --repo https://ushitora-anqou.github.io/mahout mahout mahout
	$(KUBECTL) apply -f $(WD)/manifests/postgres.yaml

.PHONY: start-elk
start-elk:
	$(KUBECTL) apply -f $(WD)/manifests/elk.yaml

.PHONY: start-mastodon
start-mastodon:
	cat $(WD)/manifests/mastodon.yaml | sed "s/E2E_TEST_MASTODON_SERVER_NAME/$(shell cat $(WD)/_test_mastodon_domain)/" | $(KUBECTL) apply -f -

.PHONY: start-waq
start-waq:
	$(MAKE) build-image
	$(KIND) load docker-image $(IMAGE) -n $(KIND_CLUSTER_NAME)
	$(KUBECTL) delete -f $(WD)/manifests/waq.yaml || true
	$(KUBECTL) delete -f $(WD)/manifests/reset-waq-database.yaml || true
	cat $(WD)/manifests/waq.yaml | sed "s/E2E_TEST_WAQ_SERVER_NAME/$(shell cat $(WD)/_test_waq_domain)/" | $(KUBECTL) apply -f -
	$(KUBECTL) apply -f $(WD)/manifests/reset-waq-database.yaml
	$(KUBECTL) wait --for=condition=available deploy/waq-web -n e2e --timeout=5m

.PHONY: waq-port-forward
waq-port-forward:
	$(KUBECTL) port-forward -n e2e --address 0.0.0.0 svc/waq-web 58080:8000

.PHONY: mastodon-port-forward
mastodon-port-forward:
	$(KUBECTL) port-forward -n e2e --address 0.0.0.0 svc/mastodon-gateway 58081:80

.PHONY: elk-port-forward
elk-port-forward:
	$(KUBECTL) port-forward -n e2e --address 0.0.0.0 svc/elk 58082:5314
