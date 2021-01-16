{ stdenv, lib, ... }:

stdenv.mkDerivation rec {
  version = "1.0";
  pname = "mystyle";
  name = "${pname}-${version}";
  tlType = "run";
  src = ./texmf;

  dontUnpack = true;
  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    path="$out/tex/latex/mystyle/"
    mkdir -p "$path"
    cd "$src"
    cp -v *.sty "$path"
  '';

  meta = with lib; {
    description = "My LaTeX style files.";
    platforms = platforms.all;
  };
}
