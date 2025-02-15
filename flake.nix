{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.follows = "opam-nix/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";

    waq-external-repo = {
      url = "github:ushitora-anqou/waq-external-repo";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    opam-nix,
    waq-external-repo,
  } @ inputs:
    flake-utils.lib.eachDefaultSystem (
      system: let
        package = "waq";
        pkgs = nixpkgs.legacyPackages.${system};
        lib = nixpkgs.lib;
        on = opam-nix.lib.${system};
        devPackagesQuery = {
          ocaml-lsp-server = "*";
          utop = "*";
          ocamlformat = let
            # read .ocamlformat
            # cf. https://nymphium.github.io/2023/05/06/purely-functioinal-ocaml-development.html
            ocamlformatConfig = lib.strings.splitString "\n" (builtins.readFile ./.ocamlformat);
            re = builtins.match "version[[:space:]]*=[[:space:]]*([^[:space:]]*)[[:space:]]*$";
            versionLine =
              lib.lists.findFirst
              (l: builtins.isList (re l))
              (throw "no version specified in .ocamlformat")
              ocamlformatConfig;
            version = builtins.elemAt (re versionLine) 0;
          in
            version;
        };
        query =
          devPackagesQuery
          // {
            ## You can force versions of certain packages here, e.g:
            ## - force the ocaml compiler to be taken from opam-repository:
            #ocaml-base-compiler = "*";
            ## - or force the compiler to be taken from nixpkgs and be a certain version:
            ocaml-system = "*";
            ## - or force ocamlfind to be a certain version:
            # ocamlfind = "1.9.2";
          };
        src = with builtins;
          filterSource (
            path: type:
              !((type == "directory")
                && elem (baseNameOf path) [
                  ".github"
                  "e2e"
                  "test"
                  "Makefile"
                ])
          )
          ./.;
        scope =
          on.buildOpamProject' {
            repos = [on.opamRepository waq-external-repo];
            resolveArgs = {
              with-test = true;
              with-doc = true;
            };
          }
          src
          query;
        overlay = final: prev:
          (
            # disable library's test and doc
            # cf. https://nymphium.github.io/2023/05/06/purely-functioinal-ocaml-development.html
            with builtins;
              mapAttrs
              (p: _:
                if hasAttr "passthru" prev.${p} && hasAttr "pkgdef" prev.${p}.passthru
                then
                  prev.${p}.overrideAttrs (_: {
                    opam__with_test = "false";
                    opam__with_doc = "false";
                  })
                else prev.${p})
              prev
          )
          // {
            # You can add overrides here
            ${package} = prev.${package}.overrideAttrs (_: {
              # Prevent the ocaml dependencies from leaking into dependent environments
              doNixSupport = false;
              removeOcamlReferences = true;
            });
          };
        scope' = scope.overrideScope overlay;
        # The main package containing the executable
        main = scope'.${package};
        # Packages from devPackagesQuery
        devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');

        customImagemagick = pkgs.imagemagick_light.override {
          # cf. https://github.com/NixOS/nixpkgs/blob/a79cfe0ebd24952b580b1cf08cd906354996d547/pkgs/applications/graphics/ImageMagick/default.nix
          #bzip2Support = true;
          #zlibSupport = true;
          #libX11Support = true;
          #libXtSupport = true;
          #fontconfigSupport = true;
          #freetypeSupport = true;
          #ghostscriptSupport = true;
          libjpegSupport = true;
          #djvulibreSupport = true;
          #lcms2Support = true;
          #openexrSupport = true;
          #libjxlSupport = true;
          libpngSupport = true;
          #liblqr1Support = true;
          #librawSupport = true;
          #librsvgSupport = true;
          libtiffSupport = true;
          #libxml2Support = true;
          #openjpegSupport = true;
          libwebpSupport = true;
          libheifSupport = true;
          #fftwSupport = true;
        };
      in {
        formatter = pkgs.alejandra;

        legacyPackages = scope';

        packages = {
          default = main;

          docker = pkgs.dockerTools.buildLayeredImage {
            name = "ghcr.io/ushitora-anqou/waq";
            tag = "dev";
            created = "now";
            extraCommands = "mkdir -m 1777 tmp";
            contents =
              (with pkgs; [
                busybox
                iana-etc
              ])
              ++ [main];
            config = {
              Entrypoint = ["waq"];
              Env = [
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
                "IMAGEMAGICK_CONVERT_PATH=${customImagemagick}/bin/convert"
              ];
            };
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [main];
          buildInputs =
            devPackages
            ++ (with pkgs; [
              # For e2e tests
              docker
              kind
              kubectl
              kubernetes-helm
            ]);
        };
      }
    );
}
