{
  makeWrapper,
  stdenv,
  fetchFromGitHub,
}:
stdenv.mkDerivation {
  name = "kneesocks";
  src = fetchFromGitHub {
    owner = "inaz2";
    repo = "kneesocks";
    rev = "e54c447fa1a98af67872a08db18e3f0d1a2cd311";
    hash = "sha256-XUJGLIrQU0NjCOaWGT9etm/A6j08XMoIYB61tlu21dY=";
  };
  nativeBuildInputs = [makeWrapper];
  buildPhase = ''
    cc -O2 -Wall -Werror -shared -fPIC -o libkneesocks.so libkneesocks.c -ldl
  '';
  installPhase = ''
    install -D -m 755 kneesocks $out/bin/kneesocks
    install -D -m 644 libkneesocks.so $out/lib/kneesocks/libkneesocks.so

     wrapProgram $out/bin/kneesocks --prefix LD_LIBRARY_PATH : $out/lib/kneesocks
  '';
}
