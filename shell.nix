let pkgs = import <nixpkgs> {};
in with pkgs; let
    # build tools
    projectHaskellEnv =
        haskellPackages.ghcWithPackages 
            (hsPackages: with hsPackages; [
                # libraries
                hakyll
                hakyll-sass
                split
                MissingH
                colour
                kmeans
                prizm
                canonical-filepath

                cabal-install
            ]);

    dependencies = [
        stdenv
        projectHaskellEnv
    ];

in stdenv.mkDerivation {
    name = "prism";
    buildInputs = dependencies;
}

