{ mkDerivation, base, containers, hostname, stdenv, taffybar
, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-config";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers hostname taffybar xmonad xmonad-contrib
  ];
  license = stdenv.lib.licenses.bsd3;
}
