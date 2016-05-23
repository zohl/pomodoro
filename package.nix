{ pkgs, stdenv, mpg321 ? null}:

let
  pomodoro-executable = pkgs.haskellPackages.callPackage ~/src/pomodoro {};

in stdenv.mkDerivation {
  name = "pomodoro";
  src = ./res;
  buildPhases = ["installPhase"];

  installPhase = ''
    mkdir -p "$out/share/pomodoro"
    cp -R "${pomodoro-executable}/"* $out
    cp -R "$src/"* "$out/share/pomodoro"
  '';
}
