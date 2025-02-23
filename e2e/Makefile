KUBERNETES_VERSION:=1.31.0
CERT_MANAGER_CHART_VERSION:=v1.17.1
TRUST_MANAGER_CHART_VERSION:=v0.16.0
WD:=$(dir $(lastword $(MAKEFILE_LIST)))
KIND_CLUSTER_NAME=waq-test-cluster
KIND:=kind
KUBECTL:=kubectl
HELM:=helm

# IMAGE should be equal to the name in flake.nix.
IMAGE=ghcr.io/ushitora-anqou/waq:dev

.PHONY: build
build:
	dune build

.PHONY: port-forward-socks5-proxy
port-forward-socks5-proxy:
	{ \
		if lsof -i:1080 > /dev/null; then : ; else \
			$(KUBECTL) port-forward deploy/socks5-proxy 1080:1080 -n e2e & \
			sleep 1 ; \
		fi ; \
	}

.PHONY: run-test
run-test:
	$(MAKE) port-forward-socks5-proxy
	env \
		KUBECTL=$(KUBECTL) \
		MANIFESTS=$(WD)/manifests \
		OCAMLRUNPARAM=b \
		E2E_TEST_WAQ_SERVER_NAME=https://$(shell cat $(WD)/_test_waq_domain) \
		E2E_TEST_MASTODON_SERVER_NAME=https://$(shell cat $(WD)/_test_mastodon_domain) \
		SSL_CERT_FILE=_test.pem \
		EIO_BACKEND=posix \
		kneesocks dune exec src/main.exe $(TESTCASES)

.PHONY: test
test:
	$(MAKE) start-waq
	$(MAKE) start-mastodon
	$(MAKE) run-test

$(BIN):
	mkdir -p $(BIN)

KUBECONFIG := $(shell pwd)/.kubeconfig
.PHONY: $(KUBECONFIG)
$(KUBECONFIG):
	$(KIND) export kubeconfig --name $(KIND_CLUSTER_NAME) --kubeconfig=$@

.PHONY: build-image
build-image:
	nix build ..#docker
	docker load -i result

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
	$(MAKE) clean-cluster
	$(KIND) create cluster -n $(KIND_CLUSTER_NAME) --image kindest/node:v$(KUBERNETES_VERSION)
	$(MAKE) $(KUBECONFIG)
	KUBECONFIG=$(KUBECONFIG) $(HELM) install --create-namespace --namespace cert-manager --repo https://charts.jetstack.io cert-manager cert-manager --version $(CERT_MANAGER_CHART_VERSION) --set crds.enabled=true --wait
	KUBECONFIG=$(KUBECONFIG) $(HELM) install --namespace cert-manager --repo https://charts.jetstack.io trust-manager trust-manager --version $(TRUST_MANAGER_CHART_VERSION) --wait
	$(KUBECTL) patch -n kube-system cm coredns --type=json -p='[{"op":"replace","path":"/data/Corefile","value":".:53 {\n    errors\n    health {\n       lameduck 5s\n    }\n    ready\n    rewrite name mastodon.waq-e2e.anqou.net mastodon-gateway-tls.e2e.svc.cluster.local\n    rewrite name waq.waq-e2e.anqou.net waq-gateway-tls.e2e.svc.cluster.local\n    kubernetes cluster.local in-addr.arpa ip6.arpa {\n       pods insecure\n       fallthrough in-addr.arpa ip6.arpa\n       ttl 30\n    }\n    prometheus :9153\n    forward . /etc/resolv.conf {\n       max_concurrent 1000\n    }\n    cache 30\n    loop\n    reload\n    loadbalance\n}"}]'
	KUBECONFIG=$(KUBECONFIG) $(HELM) install --repo https://ushitora-anqou.github.io/magout magout-cluster-wide magout-cluster-wide --wait
	$(KUBECTL) create namespace e2e
	$(MAKE) setup-selfsigned-certs
	$(KUBECTL) apply -f $(WD)/manifests/postgres.yaml
	$(KUBECTL) apply -f $(WD)/manifests/socks5-proxy.yaml

.PHONY: start-elk
start-elk:
	$(KUBECTL) apply -f $(WD)/manifests/elk.yaml

.PHONY: start-mastodon
start-mastodon:
	cat $(WD)/manifests/mastodon.yaml | sed "s/E2E_TEST_MASTODON_SERVER_NAME/$(shell cat $(WD)/_test_mastodon_domain)/" | $(KUBECTL) apply -f -
	KUBECONFIG=$(KUBECONFIG) $(HELM) upgrade --install --namespace e2e --repo https://ushitora-anqou.github.io/magout magout magout --wait --values manifests/magout-values.yaml

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

.PHONY: logs-deploy-waq
logs-deploy-waq-web:
	$(KUBECTL) logs -n e2e deploy/waq-web

.PHONY: setup-selfsigned-certs
setup-selfsigned-certs:
	echo "mastodon.waq-e2e.anqou.net" > $(WD)/_test_mastodon_domain
	echo "waq.waq-e2e.anqou.net" > $(WD)/_test_waq_domain
	$(KUBECTL) apply -f manifests/certificate.yaml
	until $(KUBECTL) get cm -n e2e ca-bundle > /dev/null ; do sleep 1 ; done
	$(KUBECTL) get cm -n e2e ca-bundle -o json | jq -r '.data."root-certs.pem"' > _test.pem
