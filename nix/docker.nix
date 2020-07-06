let
  pkgs = import <nixpkgs> {};
  whatsxmpp = import ../. {};
in
pkgs.dockerTools.buildLayeredImage {
  name = "eu.gcr.io/etainfra/whatsxmpp";
  tag = "nix";
  contents = [ whatsxmpp ];
  config.Entrypoint = [ "${whatsxmpp}/bin/whatsxmpp" ];
  config.Env = [ "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];
}
