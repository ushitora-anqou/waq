KUBERNETES_VERSION:=1.31.0
CERT_MANAGER_CHART_VERSION:=v1.17.1
TRUST_MANAGER_CHART_VERSION:=v0.16.0
WD:=$(dir $(lastword $(MAKEFILE_LIST)))
KIND_CLUSTER_NAME=waq-test-cluster

# IMAGE should be equal to the name in flake.nix.
IMAGE=ghcr.io/ushitora-anqou/waq:dev

.PHONY: build
build:
	dune build

.PHONY: port-forward-socks5-proxy
port-forward-socks5-proxy:
	{ \
		if lsof -i:1080 > /dev/null; then : ; else \
			kubectl port-forward deploy/socks5-proxy 1080:1080 -n e2e & \
			sleep 1 ; \
		fi ; \
	}

.PHONY: run-test
run-test:
	$(MAKE) port-forward-socks5-proxy
	env \
		KUBECTL=kubectl \
		MANIFESTS=$(WD)/manifests \
		OCAMLRUNPARAM=b \
		E2E_TEST_WAQ_SERVER_NAME=https://waq.waq-e2e.anqou.net \
		E2E_TEST_MASTODON_SERVER_NAME=https://mastodon.waq-e2e.anqou.net \
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
	kind export kubeconfig --name $(KIND_CLUSTER_NAME) --kubeconfig=$@

.PHONY: build-image
build-image:
	nix build ..#docker
	docker load -i result

.PHONY: clean-cluster
clean-cluster:
	kind delete cluster -n $(KIND_CLUSTER_NAME)

.PHONY: create-cluster
create-cluster:
	$(MAKE) clean-cluster
	kind create cluster -n $(KIND_CLUSTER_NAME) --image kindest/node:v$(KUBERNETES_VERSION)
	$(MAKE) $(KUBECONFIG)
	KUBECONFIG=$(KUBECONFIG) helm install --create-namespace --namespace cert-manager --repo https://charts.jetstack.io cert-manager cert-manager --version $(CERT_MANAGER_CHART_VERSION) --set crds.enabled=true --wait
	KUBECONFIG=$(KUBECONFIG) helm install --namespace cert-manager --repo https://charts.jetstack.io trust-manager trust-manager --version $(TRUST_MANAGER_CHART_VERSION) --wait
	kubectl patch -n kube-system cm coredns --type=json -p='[{"op":"replace","path":"/data/Corefile","value":".:53 {\n    errors\n    health {\n       lameduck 5s\n    }\n    ready\n    rewrite name mastodon.waq-e2e.anqou.net mastodon-gateway-tls.e2e.svc.cluster.local\n    rewrite name waq.waq-e2e.anqou.net waq-gateway-tls.e2e.svc.cluster.local\n    kubernetes cluster.local in-addr.arpa ip6.arpa {\n       pods insecure\n       fallthrough in-addr.arpa ip6.arpa\n       ttl 30\n    }\n    prometheus :9153\n    forward . /etc/resolv.conf {\n       max_concurrent 1000\n    }\n    cache 30\n    loop\n    reload\n    loadbalance\n}"}]'
	KUBECONFIG=$(KUBECONFIG) helm install --repo https://ushitora-anqou.github.io/magout magout-cluster-wide magout-cluster-wide --wait
	kubectl create namespace e2e
	$(MAKE) setup-selfsigned-certs
	kubectl apply -f $(WD)/manifests/postgres.yaml
	kubectl apply -f $(WD)/manifests/socks5-proxy.yaml

.PHONY: start-elk
start-elk:
	kubectl apply -f $(WD)/manifests/elk.yaml

.PHONY: start-mastodon
start-mastodon:
	KUBECONFIG=$(KUBECONFIG) helm upgrade --install --namespace e2e --repo https://ushitora-anqou.github.io/magout magout magout --wait --values manifests/magout-values.yaml

.PHONY: start-waq
start-waq:
	$(MAKE) build-image
	kind load docker-image $(IMAGE) -n $(KIND_CLUSTER_NAME)
	kubectl delete -f $(WD)/manifests/waq.yaml || true
	kubectl delete -f $(WD)/manifests/reset-waq-database.yaml || true
	kubectl apply -f $(WD)/manifests/reset-waq-database.yaml
	kubectl wait --for=condition=available deploy/waq-web -n e2e --timeout=5m

.PHONY: logs-deploy-waq
logs-deploy-waq-web:
	kubectl logs -n e2e deploy/waq-web

.PHONY: setup-selfsigned-certs
setup-selfsigned-certs:
	kubectl apply -f manifests/certificate.yaml
	until kubectl get cm -n e2e ca-bundle > /dev/null ; do sleep 1 ; done
	kubectl get cm -n e2e ca-bundle -o json | jq -r '.data."root-certs.pem"' > _test.pem
