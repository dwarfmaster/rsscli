{ mkDerivation, base, bytestring, dhall, download, feed, mtl
, process, relude, sqlite-simple, stdenv, utf8-string
}:
mkDerivation {
  pname = "rsscli";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring dhall download feed mtl process relude
    sqlite-simple utf8-string
  ];
  homepage = "https://github.com/lucas8/rsscli";
  description = "A newsboat compatible rss cli";
  license = stdenv.lib.licenses.mit;
  
  shellHook = ''
    export HIE_HOOGLE_DATABASE="$(readlink -f $(whereis ghc | cut -d' ' -f 2) | xargs dirname)/../share/doc/hoogle/index.html"
  '';
}
