{ mkDerivation, base, bytestring, cereal, directory, heredoc
, libnotify, network, process, stdenv, time, unix, wx, wxcore
, filepath
}:
mkDerivation {
  pname = "pomodoro";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cereal directory heredoc libnotify network process
    time unix wx wxcore
  ];
  description = "pomodoro timer";
  license = stdenv.lib.licenses.gpl3;
}
