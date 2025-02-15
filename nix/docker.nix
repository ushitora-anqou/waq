{
  busybox,
  cacert,
  dockerTools,
  iana-etc,
  imagemagick_light,
  lib,
  waq,
}: let
  customImagemagick = imagemagick_light.override {
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
in
  dockerTools.buildLayeredImage {
    name = "ghcr.io/ushitora-anqou/waq";
    tag = "dev";
    created = "now";
    extraCommands = "mkdir -m 1777 tmp";
    contents = [
      busybox
      iana-etc
      waq
    ];
    config = {
      Entrypoint = ["waq"];
      #User = "waq:waq";
      Env = [
        "SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt"
        "IMAGEMAGICK_CONVERT_PATH=${customImagemagick}/bin/convert"
      ];
    };
  }
