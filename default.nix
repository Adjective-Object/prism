{ mkDerivation, base, bytestring, canonical-filepath, colour
, JuicyPixels, kmeans, MissingH, prizm, stdenv
}:
mkDerivation {
  pname = "prism";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring canonical-filepath colour JuicyPixels kmeans
    MissingH prizm
  ];
  homepage = "https://github.com/Adjective-Object/prism";
  description = "A simple command line tool for generating colour palettes from images";
  license = stdenv.lib.licenses.asl20;
}
